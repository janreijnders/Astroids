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
timeHandler time world@World{..} = world {rndGen = snd $ nextRndGen, player = update player, enemies = map updatePosition enemies, projectiles = map updatePosition projectiles}
    where
        nextRndGen          = next rndGen
        update :: Player -> Player
        update p@Entity{..} = updatePosition $ accelerate movementAction $ rotate rotateAction player
            where
                accelerate NoMovement p         = p
                accelerate Thrust  p@Entity{..} = p {speed = speed + (mulSV acceleration $ unitVectorAtAngle $ argV direction)}
                rotate NoRotation  p            = p
                rotate RotateLeft  p@Entity{..} = p {direction =    rotationSpeed  `rotateV` direction}
                rotate RotateRight p@Entity{..} = p {direction = (- rotationSpeed) `rotateV` direction}
        updatePosition e@Entity{..} = e {position = newPosition}
            where newPosition = speed + position
