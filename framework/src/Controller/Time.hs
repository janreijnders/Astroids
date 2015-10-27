{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Debug.Trace

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point (pointInBox)
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
        or' [] = True
        or' xs = or xs
        posNP              = position newPlayer
        inBounds entity    = f $ position entity
            where f (x, y) = if x > resolutionX || x < (- resolutionX) ||
                                y > resolutionY || y < (- resolutionY) then
                                False else True
        newEnemies' = filter (\e -> or' $ map (\p -> not $ p `inside` e) projectiles) enemies
        
        inside p e = pointInBox (position p) topLeft bottomRight
                    where
                        topLeft     = ep + es
                        bottomRight = ep - es
                        ep          = position e
                        es          = (enemyScale e / 2, enemyScale e / 2)
        newEnemies | rndNum `mod` 60 /= 0 = map updateAlien 
                                            newEnemies'
                   | otherwise            = Enemy pos spd dir typ scl :
                                            map updateAlien
                                            newEnemies'
            where
                updateAlien e@Enemy{..} | enemyType == Asteroid = e
                                        | enemyType == Alien    = e {
                                   direction = normalizeV $ posNP - position,
                                   speed     = speed + 0.0001 `mulSV` direction}
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
                scl = fst $ randomR (minEnemyScale, maxEnemyScale) rndGen
        newProjectiles DontShoot = filter inBounds projectiles ++ enemyProjecs
        newProjectiles Shoot     = Projectile posNP spd dir :
                                   filter inBounds projectiles ++ enemyProjecs
            where
                spd = speed newPlayer + projectileSpeed `mulSV` dir
                dir = direction newPlayer
        enemyProjecs = mapMaybe mkProjectile enemies
        mkProjectile e = if enemyType e == Alien && rndNum * truncate
                         (magV (position e)) `mod` 480 == 0 then Just $
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
