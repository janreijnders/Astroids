{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector

import Model

-- | Drawing

playerModel = color white $ polygon [(0, 10), (-5, -5), (5, -5)]


draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{..})
    = uncurry translate (position player) $ rotate (1/2 * pi +(radToDeg $ argV $ direction player)) playerModel
