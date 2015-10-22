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

playerModel     = color white $ polygon [(0, 10), (-5, -5), (5, -5)]
enemyModel      = color white $ polygon [(0, 0), (0, 5), (5, 5), (5, 0)]
projectileModel = color white $ polygon [(0, 0), (0, 2), (2, 2), (2, 0)]
ui              = Blank
background      = Blank

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{..})
    = pictures [drawBackground, drawUI, map (\x -> drawEntity x enemyModel) enemies, map map (\x -> drawEntity x projectileModel) projectiles, drawEntity playerModel]
        where
            drawEntity e = uncurry translate (position e) $ rotate (radToDeg (1/2 * pi - (argV $ direction e)))
