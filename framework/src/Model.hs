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
        projectiles      :: [Entity]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
    deriving (Eq)
    
data Entity         = Player {
        position    :: Point, -- Coördinate the x, y plane.
        speed       :: Vector,-- The length is the absolute speed, the direction is the direction of the movement.
        direction   :: Vector -- As direction vector, make sure it is nomalized.
    }
                    | Enemy {
        position    :: Point, -- Coördinate the x, y plane.
        speed       :: Vector,-- The length is the absolute speed, the direction is the direction of the movement.
        direction   :: Vector, -- As direction vector, make sure it is nomalized.
        enemyType   :: EnemyType        
    }
                    | Projectile {
        position    :: Point, -- Coördinate the x, y plane.
        speed       :: Vector,-- The length is the absolute speed, the direction is the direction of the movement.
        direction   :: Vector -- As direction vector, make sure it is nomalized.
    }

data EnemyType = AsteroidSmall | AsteroidBig | AlienSmall | AlienBig
    deriving (Bounded, Enum, Eq, Ord)

-- In unit lengths per frame
acceleration :: Float
acceleration = 0.1
-- In fraction of speed lost per frame
-- This value combined with the acceleration value implicitly gives the player a maximum speed
-- For realism set keep this 0, for better gameplay increase this value
deceleration :: Float
deceleration = 0.01
-- In radians
rotationSpeed :: Float
rotationSpeed = 1/32 * pi
-- In unit lengths per frame
projectileSpeed :: Float
projectileSpeed = 5.0
-- In unit lengths per frame
defaultEnemySpeed :: Float
defaultEnemySpeed = 3.0

initial :: Int -> World
initial seed = World {rndGen = mkStdGen seed, rotateAction = NoRotation, movementAction = NoMovement, shootAction = DontShoot, player = defaultPlayer, enemies = [], projectiles = []}
    where
        defaultPlayer = Player {position = (0, 0), speed = (0, 0), direction = (0, 1)}
