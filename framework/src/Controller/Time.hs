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
        randomList         = randoms rndGen :: [Int]
        rndNum             = fst $ random rndGen :: Int
        rndGens            = split rndGen
        inBounds entity    = f $ position entity
            where f (x, y) = if x > resolutionX || x < (- resolutionX) ||
                                y > resolutionY || y < (- resolutionY) then
                                False else True
        newEnemies | rndNum `mod` 60 /= 0 =                  filter keep enemies
                   | otherwise = Enemy pos spd dir typ scl : filter keep enemies
            where
                keep    enemy = (isAlive enemy) && (inBounds enemy)
                isAlive :: Entity -> Bool
                isAlive enemy = or (map (\p -> p `inside` enemy) projectiles)
                inside :: Entity -> Entity -> Bool
                inside p e    = fst (position p) < fst (position e) + enemyScale e &&
                                fst (position p) > fst (position e) - enemyScale e &&
                                snd (position p) < snd (position e) + enemyScale e &&
                                snd (position p) > snd (position e) - enemyScale e
                pos | fst $ random $ fst rndGens =
                                   (rndNegative * resolutionX / 2, fst $ randomR
                                   (- resolutionY / 2, resolutionY / 2) rndGen)
                    | otherwise = (fst $ randomR (- resolutionX / 2, resolutionX
                                  / 2) rndGen, rndNegative * resolutionY / 2)
                    where rndNegative = if fst $ random $ snd rndGens then -1
                                                                      else  1
                spd = (fst $ randomR (minEnemySpeed, maxEnemySpeed) rndGen)
                      `mulSV` dir
                dir = normalizeV $ position player - pos
                typ = fst $ random rndGen -- TODO make weigthed so aliens are more rare
                scl = fst $ randomR (minEnemyScale, maxEnemyScale) rndGen
        newProjectiles DontShoot = filter inBounds projectiles
        newProjectiles Shoot     = Projectile pos spd dir :
                                   filter inBounds projectiles
            where
                pos = position newPlayer
                spd = (speed newPlayer) + projectileSpeed `mulSV`
                      unitVectorAtAngle (argV $ direction newPlayer)
                dir = direction newPlayer
        newExhausts NoMovement   = take (length exhausts - 15) exhausts
        newExhausts Thrust       = map mkExhaust (take 15 randomList)
                                   ++ take 15 exhausts
            where                
                mkExhaust n      = Exhaust (position newPlayer) ((fromIntegral -- TODO clean this
                                   (n `mod` 499) / 50) `mulSV` (argV (direction
                                   newPlayer) `rotateV` unitVectorAtAngle
                                   (getNum n))) (0, 0)
                getNum a         = (fromIntegral (a `mod` 1009) / 2016 + 3/4)
                                   * pi
        newPowerUps              = powerUps -- TODO add random powerUps
        newPlayer                = update player -- TODO prevent outofbounds exception
        update p@Player{..}      = updatePosition $ accelerate movementAction
                                             $ rotate rotateAction player
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
