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
asteroidModel   = color white $ polygon [(-1, -1), (-1, 1), (1, 1), (1, -1)] -- TODO improve model
alienModel      = color white $ polygon [(0, 5), (-5, -5), (5, -5)] -- TODO improve model
projectileModel = color red $ polygon [(0, 0), (0, 2), (2, 2), (2, 0)] -- TODO improve model
particleModel   = color yellow $ polygon [(0, 0), (0, 1), (1, 1), (1, 0)] -- TODO improve model
powerUpModel    = color green $ polygon [(-10, -10), (-10, 10), (10, 10), (10, -10)] -- TODO add powerUp model
starModel       = color white $ polygon [(0, 0), (0, 1), (1, 1), (1, 0)] -- TODO improve model

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{..})
    = pictures [ui,
                pictures $ map drawEntity enemies,
                pictures $ map drawEntity projectiles,
                pictures $ map drawEntity exhausts,
                pictures $ map drawEntity explosions,
                pictures $ map drawEntity powerUps,
                pictures $ map drawStar stars,
                drawEntity player]
        where
            drawEntity p@Player{..} = if alive then drawEntity' p playerModel
                                               else Blank
            drawEntity e@Enemy{..}  = drawEntity' e (scale (entityScale / 2)
                                    (entityScale / 2) (getEnemyModel enemyType))
                where
                    getEnemyModel Asteroid = asteroidModel
                    getEnemyModel Alien    = scale 0.05 0.05 alienModel
            drawEntity p@Projectile{..} = drawEntity' p projectileModel
            drawEntity e@Particle{..}   = drawEntity' e particleModel
            drawEntity p@PowerUp{..}    = drawEntity' p powerUpModel
            drawEntity' entity model    = uncurry translate (position entity) $
                                          rotate (radToDeg (1/2 * pi -
                                          argV (direction entity))) model
            drawStar (Vector3 x y _)    = Translate x y starModel
            ui = translate (- resolutionX / 2.5) (- resolutionY / 2.5) $ scale
                 0.1 0.1 $ Color white $ Text ("Score: " ++ show
                 (score gameState) ++ " Multiplier: " ++ show (scoreMultiplier
                                                                     gameState))
