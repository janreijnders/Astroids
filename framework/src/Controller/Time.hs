{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Data.Maybe
import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@World{..} = world {
    rndGen      = snd $ next rndGen,
    player      = newPlayer,
    enemies     = map updatePosition newEnemies,
    projectiles = map updatePosition $ newProjectiles shootAction,
    exhausts    = map updatePosition $ newExhausts movementAction,
    powerUps    = map updatePosition newPowerUps,
    shootAction = DontShoot }
    where
        randomList         = randomRs (0, 1000) rndGen :: [Int]
        rndNum             = fst $ random rndGen :: Int
        rndGens            = split rndGen
        posNP              = position newPlayer
        inBounds entity    = f $ position entity
            where f (x, y) = if x > resolutionX || x < (- resolutionX) ||
                                y > resolutionY || y < (- resolutionY) then
                                False else True
        newEnemies | rndNum `mod` 60 /= 0 = map updateAlien 
                                            (filter keep enemies)
                   | otherwise            = Enemy pos spd dir typ :
                                            map updateAlien
                                            (filter keep enemies)
            where
                updateAlien e@Enemy{..} | enemyType == Asteroid = e
                                        | enemyType == Alien    = e {direction =
                                       normalizeV $ posNP - position,
                                       speed = speed + 0.0001 `mulSV` direction}
                keep    enemy = isAlive enemy && inBounds enemy
                isAlive enemy = True -- TODO detect if enemy is hit 
                pos | fst $ random $ fst rndGens =
                                   (rndNegative * resolutionX / 2, fst $ randomR
                                   (- resolutionY / 2, resolutionY / 2) rndGen)
                    | otherwise = (fst $ randomR (- resolutionX / 2, resolutionX
                                  / 2) rndGen, rndNegative * resolutionY / 2)
                    where rndNegative = if fst $ random $ snd rndGens then -1
                                                                      else  1
                spd = (fst $ randomR (minEnemySpeed, maxEnemySpeed) rndGen)
                      `mulSV` dir
                dir = normalizeV $ position newPlayer - pos
                typ = fst $ random rndGen -- TODO make weigthed so aliens are more rare
        newProjectiles DontShoot = filter inBounds projectiles ++ enemyProjecs
        newProjectiles Shoot     = Projectile posNP spd dir :
                                   filter inBounds projectiles ++ enemyProjecs
            where
                spd = speed newPlayer + projectileSpeed `mulSV` dir
                dir = direction newPlayer
        enemyProjecs = mapMaybe mkProjectile enemies
        mkProjectile e = if enemyType e == Alien && rndNum * truncate
                         (magV (position e)) `mod` 300 == 0 then Just $
                         Projectile pos' spd' dir' else Nothing
            where
                pos' = position e
                spd' = speed e + projectileSpeed `mulSV` dir'
                dir' = direction e
        newExhausts NoMovement   = take (length exhausts - 15) exhausts
        newExhausts Thrust       = map mkExhaust (take 15 randomList)
                                   ++ take 15 exhausts
            where                
                mkExhaust :: Int -> Entity
                mkExhaust n = Exhaust pos spd dir
                    where
                        pos = position newPlayer
                        spd = (fromIntegral (n `mod` 100) / 20) `mulSV` dir
                        dir = ((fromIntegral n / 2000 + 3 / 4) * pi) `rotateV`
                              direction newPlayer
        newPowerUps         = powerUps -- TODO add random powerUps
        newPlayer           = update player -- TODO prevent outofbounds exception
        update p@Player{..} = updatePosition $ accelerate movementAction
                              $ rotate rotateAction p
            where
                accelerate NoMovement p@Player{..} = p
                    {speed = if magV speed < 0.05 then (0, 0)
                             else (1 - deceleration) `mulSV` speed}
                accelerate Thrust     p@Player{..} = p
                    {speed = (1 - deceleration) `mulSV` (speed +
                    (mulSV acceleration $ unitVectorAtAngle $ argV direction))}
                rotate NoRotation     p            = p
                rotate RotateLeft     p@Player{..} = p
                    {direction =    rotationSpeed  `rotateV` direction}
                rotate RotateRight    p@Player{..} = p
                    {direction = (- rotationSpeed) `rotateV` direction}
        updatePosition e = e {position = newPosition}
            where newPosition = speed e + position e
