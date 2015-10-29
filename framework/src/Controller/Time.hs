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
-- TODO improve enemy hitboxes (they do not rotate with the enemy) and keep score
timeHandler :: Float -> World -> World
timeHandler time world@World{..} 
    | not (alive player) = world {
        rndGen      = snd $ next rndGen,
        enemies     = map updatePosition enemies,
        projectiles = map updatePosition projectiles,
        explosions  = map updatePosition newExplosions}
    | playerIsHit = world {
        rndGen      = snd $ next rndGen,
        player      = player {alive = False},
        enemies     = map updatePosition enemies,
        projectiles = map updatePosition projectiles,
        exhausts    = [],
        powerUps    = [],
        explosions  = map updatePosition (newExplosions ++ playerExplosion)}
    | otherwise = world {
        rndGen      = snd $ next rndGen,
        player      = newPlayer,
        enemies     = map updatePosition newEnemies,
        projectiles = map updatePosition $ newProjectiles shootAction,
        exhausts    = map updatePosition $ newExhausts movementAction,
        powerUps    = newPowerUps,
        explosions  = map updatePosition newExplosions,
        shootAction = DontShoot,
        nextID      = if spawnEnemy then nextID + 1 else nextID,
        stars       = map updateStar newStars}
    where
        playerIsHit     = (or' $ map (player `inside`) enemies) || (or' $ map
                          (\p -> pointInBox (position p) (position player +
                          (4, 4)) (position player - (4, 4))) projectiles)
        randomList      = randomRs (0, 1000) rndGen :: [Int]
        rndNum          = fst $ random rndGen :: Int
        rndGens         = split rndGen
        posNP           = position newPlayer
        spawnEnemy      = yesNo spawnChance rndGen
        inBounds entity = f $ position entity
            where f (x, y) = not 
                             (x > resolutionX / 2 || x < (- resolutionX / 2) ||
                              y > resolutionY / 2 || y < (- resolutionY / 2))
        inBoundsStar (Vector3 x y _) = not
                             (x > resolutionX / 2 || x < (- resolutionX / 2) ||
                              y > resolutionY / 2 || y < (- resolutionY / 2))
        enemyProjectileList = [(e, p) | e <- enemies, p <- projectiles,
                              (p `inside` e) && (shooter p /= entityID e)]
        inside p e = pointInBox (position p) topLeft bottomRight
                    where
                        topLeft     = ep + es
                        bottomRight = ep - es
                        ep          = position e
                        es          = (enemyScale e, enemyScale e)
        newStars = filtered ++ replacements
                  where
                    filtered     = filter inBoundsStar stars
                    replacements = take (1000 - length filtered) (randomRs
                                   ((Vector3 (- resolutionX / 2) (- resolutionY
                                   / 2) 1000), (Vector3 (- resolutionX/2)
                                   (resolutionY/2) 10000)) rndGen)                    
        newEnemies | spawnEnemy = Enemy pos spd dir typ scl nextID :
                                  map updateAlien newEnemies'
                   | otherwise  = map updateAlien newEnemies'
            where
                newEnemies' = filter inBounds ((filter (\e -> not $ elem e
                              (map fst enemyProjectileList)) enemies))
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
                typ | yesNo alienChance rndGen = Alien
                    | otherwise = Asteroid
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
        newPowerUps | yesNo powerUpChance rndGen = mkPowerUp : powerUps
                    | otherwise    =             powerUps
            where
                mkPowerUp = PowerUp (fst $ randomR (- resolutionX / 2,
                            resolutionX / 2) (fst rndGens), fst $ randomR
                            (- resolutionY / 2, resolutionY / 2) (snd rndGens))
                            (0, 0)
        newExplosions = concatMap (mkExplosion . fst) enemyProjectileList
                        ++ filter inBounds explosions
            where
                mkExplosion e@Enemy{..} = map f (zip (take (truncate enemyScale
                                          * 10) (randomRs (1, 10) (fst rndGens))
                                          ) (take (truncate enemyScale * 10)
                                          (randomRs (0, 2 * pi) (snd rndGens))))
                    where
                        f (spd', dir') = Exhaust position (spd + speed) dir
                            where
                                spd = spd' `mulSV` dir
                                dir = unitVectorAtAngle dir'
        playerExplosion = map f (zip (take 2000 (randomRs (1, 10) (fst rndGens))
                          ) (randomRs (0, 2 * pi) (snd rndGens)))
                    where
                        f (spd', dir') = Exhaust (position player) (spd + speed
                                         player) dir
                            where
                                spd = spd' `mulSV` dir
                                dir = unitVectorAtAngle dir'
        newPlayer           = if not $ inBounds newPlayer'
                              then rotate rotateAction $ player {speed = (0, 0)}
                              else newPlayer'
        newPlayer'          = update player
        update p@Player{..} = updatePosition $ accelerate movementAction
                              $ rotate rotateAction p
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
        updateStar (Vector3 x y z) = (Vector3 updatedX y z)
            where 
                updatedX = x + 2 * (scrollDistance / (2 * z)) * (horizon - z)

updatePosition :: Entity -> Entity
updatePosition e = e {position = newPosition}
            where newPosition = speed e + position e

or' :: [Bool] -> Bool
or' [] = False
or' xs = or xs

yesNo :: (Int, Int) -> StdGen -> Bool
yesNo (a, b) g = fst (randomR (1, b) g) <= a

inBounds' :: Num a => Ord a => (a, a) -> (a, a) -> Bool
inBounds' (x, y) (boundX, boundY)  = not (x > boundX || x < (- boundX) ||
                                         y > boundY || y < (- boundY))
