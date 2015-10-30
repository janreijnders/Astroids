module Controller.Event (
    eventHandler
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Model

-- | Event handling

eventHandler :: Event -> World -> World
eventHandler (EventKey (SpecialKey KeyLeft ) Down _ _) world
    = world { rotateAction = RotateLeft }
eventHandler (EventKey (SpecialKey KeyLeft ) Up   _ _) world
    = world { rotateAction = NoRotation }
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) world
    = world { rotateAction = RotateRight }
eventHandler (EventKey (SpecialKey KeyRight) Up   _ _) world
    = world { rotateAction = NoRotation }
eventHandler (EventKey (SpecialKey KeyUp   ) Down _ _) world
    = world { movementAction = Thrust }
eventHandler (EventKey (SpecialKey KeyUp   ) Up   _ _) world
    = world { movementAction = NoMovement }
eventHandler (EventKey (SpecialKey KeySpace) Down _ _) world
    = world { shootAction = Shoot }
eventHandler (EventKey (SpecialKey KeySpace) Up   _ _) world
    = world { shootAction = DontShoot }
eventHandler (EventKey (SpecialKey KeyEnter) Down  _ _) world
    = initial ( fst $ next $ rndGen world) (resolutionX world) (resolutionY world)
eventHandler _ world
    = world
