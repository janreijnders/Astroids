{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Picture

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        resolutionX      :: Float,
        resolutionY      :: Float,
        player           :: Entity,
        enemies          :: [Entity],
        projectiles      :: [Entity],
        exhausts         :: [Entity],
        powerUps         :: [Entity],
        explosions       :: [Entity],
        gameState        :: GameState,
        nextID           :: Int,
        stars            :: [Vector3]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
    deriving (Eq)
data Vector3 = Vector3 Float Float Float
    deriving (Show)
data GameState = GameState {
        score           :: Int,
        scoreMultiplier :: Int
    }
    
data Entity         = Player {
        position    :: Point, -- Coördinate the x, y plane.
        speed       :: Vector,-- The length is the absolute speed, the direction
                              -- is the direction of the movement.
        direction   :: Vector,-- As direction vector, make sure it is nomalized.
        alive       :: Bool
    }
                    | Enemy {
        position    :: Point,
        speed       :: Vector,
        direction   :: Vector,
        enemyType   :: EnemyType,
        enemyScale  :: Float,
        entityID    :: Int
    }
                    | Projectile {
        position    :: Point,
        speed       :: Vector,
        direction   :: Vector,
        shooter     :: Int
    }
                    | Exhaust {
        position    :: Point,
        speed       :: Vector,
        direction   :: Vector
    }
                    | PowerUp {
        position    :: Point,
        direction   :: Vector
    }
    deriving (Eq)

data EnemyType = Asteroid | Alien
    deriving (Bounded, Enum, Eq, Ord)

instance Random Vector3 where 
    randomR (Vector3 lx ly lz, Vector3 hx hy hz) g =
        (Vector3 (fst int1) (fst int2) (fst int3), snd int3)
        where 
            int1 = randomR (lx, hx) g
            int2 = randomR (ly, hy) (snd int1)
            int3 = randomR (lz, hz) (snd int2)
    random = randomR (Vector3 0 0 0, Vector3 1 1 1)
    
instance Random EnemyType where
    randomR (l, h) g = f (randomR (fromEnum l, fromEnum h) g)
        where f (enemyType, rng) = (toEnum enemyType, rng)
    random           = randomR (minBound, maxBound)
    
-- In unit lengths per frame.
acceleration :: Float
acceleration = 0.09
-- In fraction of speed lost per frame.
-- This value combined with the acceleration value implicitly gives the player a
-- maximum speed. For realism set this to 0, for better gameplay increase this
-- value.
deceleration :: Float
deceleration = 0.01
-- In radians
rotationSpeed :: Float
rotationSpeed = 1/32 * pi
-- In unit lengths per frame
projectileSpeed :: Float
projectileSpeed = 8.0
-- In unit lengths per frame
minEnemySpeed :: Float
minEnemySpeed = 1.0
-- In unit lengths per frame
maxEnemySpeed :: Float
maxEnemySpeed = 5.0
-- In unit lengths
minEnemyScale :: Float
minEnemyScale = 10.0
-- In unit lengths
maxEnemyScale :: Float
maxEnemyScale = 100.0
-- There is a chance of 1 to this value for an enemy to spawn each frame
spawnChance :: Int
spawnChance = 60
-- There is a chance of 1 to this value for a powerUp to spawn each frame
powerUpChance :: Int
powerUpChance = 180
-- There is a chance of 1 to this value for each enemy to shoot each frame
shootChance :: Int
shootChance = 360
-- In unit lengths
scrollDistance = 0.001 :: Float
horizon = 1000000 :: Float

initial :: Int -> Float -> Float -> World
initial seed x y = World {
            rndGen = mkStdGen seed, rotateAction = NoRotation,
            movementAction = NoMovement, shootAction = DontShoot,
            resolutionX = x, resolutionY = y, player = defaultPlayer,
            enemies = [], projectiles = [], exhausts = [], powerUps = [],
            explosions = [], gameState = defaultGameState, nextID = 1,
            stars = randomStars }
    where
        defaultPlayer    = Player { position  = (0, 0), speed = (0, 0),
                                    direction = (0, 1), alive = True  }
        defaultGameState = GameState {score = 0, scoreMultiplier = 0}
        randomStars = take 1000 (randomRs ((Vector3 (- x / 2) (- y / 2) 1000),
                      (Vector3 (x / 2) (y / 2) 10000)) (mkStdGen seed))
