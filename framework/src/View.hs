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

playerModel     = color white $ polygon [(0, 10), (-5, -5), (5, -5)] -- TODO improve model
asteroidModel   = color white $ polygon [(0, 0), (0, 5), (5, 5), (5, 0)] -- TODO improve model
alienModel      = color white $ polygon [(0, 0), (0, 5), (5, 5), (5, 0)] -- TODO improve model
projectileModel = color white $ polygon [(0, 0), (0, 2), (2, 2), (2, 0)] -- TODO improve model
exhaustModel    = color white $ polygon [(0, 0), (0, 1), (1, 1), (1, 0)] -- TODO improve model
powerUpModel    = Blank -- TODO add powerUp model
ui              = Blank -- TODO add score and other stuff
background      = Blank -- TODO add stars with paralax

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{..})
    = pictures [background, ui,
                pictures $ map drawEntity enemies,
                pictures $ map drawEntity projectiles,
                pictures $ map drawEntity exhausts,
                drawEntity player]
        where
            drawEntity p@Player{..} = drawEntity' p playerModel
            drawEntity e@Enemy{..}  = drawEntity' e (getEnemyModel enemyType)
                where
                    getEnemyModel asteroidSmall = asteroidModel
                    getEnemyModel asteroidBig   = scale 2 2 asteroidModel
                    getEnemyModel alienSmall    = alienModel
                    getEnemyModel alienBig      = scale 2 2 alienModel
            drawEntity p@Projectile{..} = drawEntity' p projectileModel
            drawEntity e@Exhaust{..}    = drawEntity' e exhaustModel
            drawEntity p@PowerUp{..}    = drawEntity' p powerUpModel
            drawEntity' entity model    = uncurry translate (position entity) $
                                          rotate (radToDeg (1/2 * pi -
                                          (argV $ direction entity))) model