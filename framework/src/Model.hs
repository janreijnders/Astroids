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
        player           :: Entity,
        enemies          :: [Entity],
        projectiles      :: [Entity],
        exhausts         :: [Entity],
        powerUps         :: [Entity],
        gameState        :: GameState
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
        direction   :: Vector -- As direction vector, make sure it is nomalized.
    }
                    | Enemy {
        position    :: Point,
        speed       :: Vector,
        direction   :: Vector,
        enemyType   :: EnemyType        
    }
                    | Projectile {
        position    :: Point,
        speed       :: Vector,
        direction   :: Vector
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

data EnemyType = AsteroidSmall | AsteroidBig | AlienSmall | AlienBig
    deriving (Bounded, Enum, Eq, Ord)

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
defaultEnemySpeed :: Float
defaultEnemySpeed = 3.0

initial :: Int -> World
initial seed = World {
            rndGen = mkStdGen seed, rotateAction = NoRotation,
            movementAction = NoMovement, shootAction = DontShoot,
            player = defaultPlayer, enemies = [], projectiles = [],
            exhausts = [], powerUps = [], gameState = defaultGameState }
    where
        defaultPlayer    = Player {
                        position = (0, 0), speed = (0, 0), direction = (0, 1)}
        defaultGameState = GameState {
                        score = 0, scoreMultiplier = 0}
