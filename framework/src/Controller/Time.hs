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
-- TODO make dead enemies explode, detect if the player gets hit, improve enemy hitboxes and keep score
timeHandler :: Float -> World -> World
timeHandler time world@World{..} = world {
    rndGen      = snd $ next rndGen,
    player      = newPlayer,
    enemies     = map updatePosition newEnemies,
    projectiles = map updatePosition $ newProjectiles shootAction,
    exhausts    = map updatePosition $ newExhausts movementAction,
    powerUps    = map updatePosition newPowerUps,
    shootAction = DontShoot,
    nextID      = if spawnEnemy then nextID + 1 else nextID }
    where
        randomList      = randomRs (0, 1000) rndGen :: [Int]
        rndNum          = fst $ random rndGen :: Int
        rndGens         = split rndGen
        or' []          = False
        or' xs          = or xs
        posNP           = position newPlayer
        spawnEnemy      = fst (randomR (0, spawnChance) rndGen) == 0
        inBounds entity = f $ position entity
            where f (x, y) = not (x > resolutionX || x < (- resolutionX) ||
                                  y > resolutionY || y < (- resolutionY))
        enemyProjectileList = [(e, p) | e <- enemies, p <- projectiles,
                              (p `inside` e) && (shooter p /= entityID e)]
        inside p e = pointInBox (position p) topLeft bottomRight
                    where
                        topLeft     = ep + es
                        bottomRight = ep - es
                        ep          = position e
                        es          = (enemyScale e, enemyScale e)
        newEnemies | spawnEnemy = Enemy pos spd dir typ scl nextID :
                                  map updateAlien newEnemies'
                   | otherwise  = map updateAlien newEnemies'
            where
                newEnemies' = (filter (\e -> not $ elem e
                              (map fst enemyProjectileList)) enemies)
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
                spd = fst (randomR (minEnemySpeed, maxEnemySpeed) rndGen)
                      `mulSV` dir
                dir = normalizeV $ position newPlayer - pos
                typ = fst $ random rndGen -- TODO make weigthed so aliens are more rare
                scl = fst $ randomR (minEnemyScale, maxEnemyScale) rndGen
        newProjectiles' = filter inBounds (filter (\p -> not $ elem p (map snd
                          enemyProjectileList)) projectiles) ++ enemyProjecs
        newProjectiles DontShoot =                              newProjectiles'
        newProjectiles Shoot     = Projectile posNP spd dir 0 : newProjectiles'
            where
                
                spd = speed newPlayer + projectileSpeed `mulSV` dir
                dir = direction newPlayer
        enemyProjecs = mapMaybe mkProjectile enemies
        mkProjectile e = if enemyType e == Alien && (entityID e * rndNum) `mod`
                         shootChance == 0 then Just $
                         Projectile pos' spd' dir' (entityID e) else Nothing
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
                    mulSV acceleration (unitVectorAtAngle $ argV direction))}
                rotate NoRotation     p            = p
                rotate RotateLeft     p@Player{..} = p
                    {direction =    rotationSpeed  `rotateV` direction}
                rotate RotateRight    p@Player{..} = p
                    {direction = (- rotationSpeed) `rotateV` direction}
        updatePosition e = e {position = newPosition}
            where newPosition = speed e + position e
