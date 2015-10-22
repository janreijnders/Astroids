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
asteroidModel   = color white $ polygon [(0, 0), (0, 5), (5, 5), (5, 0)]
alienModel      = color white $ polygon [(0, 0), (0, 5), (5, 5), (5, 0)]
projectileModel = color white $ polygon [(0, 0), (0, 2), (2, 2), (2, 0)]
ui              = Blank
background      = Blank

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{..})
    = pictures [drawBackground, drawUI, pictures $ map drawEntity enemies, pictures $ map drawEntity projectiles, drawEntity player]
        where
            drawEntity p@Player{..} = drawEntity' p playerModel
            drawEntity e@Enemy{..} = drawEntity' e (getEnemyModel enemyType)
                where
                    getEnemyModel asteroidSmall = asteroidModel
                    getEnemyModel asteroidBig   = scale 2 2 asteroidModel
                    getEnemyModel alienSmall    = alienModel
                    getEnemyModel alienBig      = scale 2 2 alienModel
            drawEntity p@Projectile{..} = drawEntity' p projectileModel
            drawEntity' entity model = uncurry translate (position entity) $ rotate (radToDeg (1/2 * pi - (argV $ direction entity))) model
            drawUI = ui
            drawBackground = background
