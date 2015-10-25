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
        randomList               = randoms rndGen :: [Int]
        newEnemies               = enemies -- TODO add random enemies and detect and delete hit enemies
        newProjectiles DontShoot = projectiles --TODO make some kind of garbage collector for projectiles that are off the screen
        newProjectiles Shoot     = Projectile pos spd dir : take 999 projectiles
            where
                pos = position newPlayer
                spd = (speed newPlayer) + projectileSpeed `mulSV`
                      unitVectorAtAngle (argV $ direction newPlayer)
                dir = direction newPlayer
        newExhausts NoMovement   = take (length exhausts - 15) exhausts
        newExhausts Thrust       = map mkExhaust (take 15 randomList)
                                   ++ take 15 exhausts
            where                
                mkExhaust n      = Exhaust (position newPlayer) ((fromIntegral
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