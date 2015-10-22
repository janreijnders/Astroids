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
        player           :: Player,
        enemies          :: [Enemy],
        projectiles      :: [Projectile]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
    deriving (Eq)
data Entity         = Entity {
        position    :: Point, -- Coördinate the x, y plane.
        speed       :: Vector,-- The length is the absolute speed, the direction is the direction of the movement.
        direction   :: Vector -- As direction vector, make sure it is nomalized.
    }
type Player     = Entity
type Enemy      = Entity
type Projectile = Entity

-- In unit lengths per frame
acceleration :: Float
acceleration = 0.01
-- In radians
rotationSpeed :: Float
rotationSpeed = 1/64 * pi
-- In unit lengths per frame
projectileSpeed :: Float
projectileSpeed = 5.0
-- In unit lengths per frame
defaultEnemySpeed :: Float
defaultEnemySpeed = 3.0

initial :: Int -> World
initial seed = World {rndGen = mkStdGen seed, rotateAction = NoRotation, movementAction = NoMovement, shootAction = DontShoot, player = defaultPlayer, enemies = [], projectiles = []}
    where
        defaultPlayer = Entity {position = (0, 0), speed = (0, 0), direction = (0, 1)}
