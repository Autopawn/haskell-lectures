import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

velPaleta = 5 :: Float
largoPaleta = 40 :: Float
distPaleta = 200 :: Float
medLenX = 250 :: Float
medLenY = 150 :: Float

dibujarRect :: Float -> Float -> Float -> Float -> Picture
dibujarRect x1 y1 x2 y2 = Polygon [(x1,y1),(x1,y2),(x2,y2),(x2,y1)]

cancha = Color cyan (dibujarRect (-medLenX) (-medLenY) (medLenX) (medLenY))

dibujarPaleta :: Float -> Float -> Picture
dibujarPaleta x y = dibujarRect (x-10) (y - largoPaleta / 2) (x+10) (y + largoPaleta / 2)
dibujarPelota :: Float -> Float -> Picture
dibujarPelota x y = dibujarRect (x-10) (y-10) (x+10) (y+10)

data Paleta = Paleta {pal_y :: Float, pal_vy :: Float}
data Pelota = Pelota Float Float Float Float

data Universo = Universo {
    paleta1 :: Paleta,
    paleta2 :: Paleta,
    pelota :: Pelota,
    score1 :: Int,
    score2 :: Int
}

procInput :: Event -> Universo -> Universo
procInput (EventKey (SpecialKey KeyUp) Down _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) + velPaleta}}
procInput (EventKey (SpecialKey KeyUp) Up _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) - velPaleta}}
procInput (EventKey (SpecialKey KeyDown) Down _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) - velPaleta}}
procInput (EventKey (SpecialKey KeyDown) Up _ _) uni =
    uni {paleta1 = (paleta1 uni) {pal_vy = pal_vy (paleta1 uni) + velPaleta}}
procInput _ uni = uni

procTiempo :: Float -> Universo -> Universo
procTiempo dt uni = let
    actualizarPal pa = pa {pal_y = pal_y pa + pal_vy pa}
    in uni {
        paleta1 = actualizarPal (paleta1 uni),
        paleta2 = actualizarPal (paleta2 uni)
    }

dibujar :: Universo -> Picture
dibujar uni = Pictures [cancha,
    dibujarPaleta (-distPaleta) (pal_y (paleta1 uni)),
    dibujarPaleta ( distPaleta) (pal_y (paleta2 uni))]

mundoi = Universo {
    paleta1 = Paleta 0 0,
    paleta2 = Paleta 0 0,
    pelota = Pelota 0 0 0 0,
    score1 = 0,
    score2 = 0
}

main = do
    play (FullScreen (1024,768)) white 60 mundoi dibujar procInput procTiempo
