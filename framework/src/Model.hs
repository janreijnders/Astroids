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
        nextID           :: Int
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
    deriving (Eq)

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

initial :: Int -> Float -> Float -> World
initial seed x y = World {
            rndGen = mkStdGen seed, rotateAction = NoRotation,
            movementAction = NoMovement, shootAction = DontShoot,
            resolutionX = x, resolutionY = y, player = defaultPlayer,
            enemies = [], projectiles = [], exhausts = [], powerUps = [],
            explosions = [], gameState = defaultGameState, nextID = 1 }
    where
        defaultPlayer    = Player { position  = (0, 0), speed = (0, 0),
                                    direction = (0, 1), alive = True  }
        defaultGameState = GameState {score = 0, scoreMultiplier = 0}
