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

projectileModel = color red $ polygon [(-2, -2), (-2, 2), (2, 2), (2, -2)]
particleModel   = polygon [(0, 0), (0, 1), (1, 1), (1, 0)]
starModel       = color white $ polygon [(0, 0), (0, 1), (1, 1), (1, 0)] 

draw :: Float -> Float -> Picture -> Picture -> Picture -> Picture -> World -> Picture
draw horizontalResolution verticalResolution spaceshipSprite enemySprite
                                  powerupSprite asteroidSprite world@(World{..}) 
    = pictures [ pictures $ map drawStar stars,
                 pictures $ map (\e -> drawEntity' e (Color orange $ particleModel)) explosions,
                 pictures $ map drawEntity enemies,
                 pictures $ map drawEntity powerUps,
                 pictures $ map drawEntity projectiles ,
                 pictures $ map (\e -> drawEntity' e (Color blue $ particleModel)) exhausts,
                 drawEntity player, ui]
        where
            drawEntity p@Player{..} = if alive then drawEntity' p spaceshipSprite
                                               else translate (- resolutionX / 3) 0
                $ scale 0.5 0.5 $ Color white $ Text "Press ENTER to restart"
            drawEntity e@Enemy{..}  = drawEntity' e (scale (entityScale / 2)
                                    (entityScale / 2) (getEnemyModel enemyType))
                where
                    getEnemyModel Asteroid = asteroidSprite
                    getEnemyModel Alien    = scale 0.05 0.05 enemySprite
            drawEntity p@Projectile{..} = drawEntity' p projectileModel
            drawEntity e@Particle{..}   = drawEntity' e particleModel
            drawEntity p@PowerUp{..}    = drawEntity' p powerupSprite
            drawEntity' entity model    = uncurry translate (position entity) $
                                          rotate (radToDeg (1/2 * pi -
                                          argV (direction entity))) model
            drawStar (Vector3 x y _)    = Translate x y starModel
            ui = translate (- resolutionX / 2.5) (- resolutionY / 2.5) $ scale
                 0.1 0.1 $ Color white $ Text ("Score: " ++ show
                 (score gameState) ++ " Multiplier: " ++ show (scoreMultiplier
                                                                     gameState))
