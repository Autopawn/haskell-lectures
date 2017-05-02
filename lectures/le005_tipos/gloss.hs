import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

data Pelota = Pelota Float Float Float Float

type Model = [Pelota]

actPelota :: Float -> Pelota -> Pelota
actPelota dt (Pelota x y vx vy) = let
    vx' = if x <= -300 || x >= 300 then -vx else vx
    vy' = if y <= -200 || y >= 200 then -vy else vy
    in Pelota (x+vx'*dt) (y+vy'*dt) vx' (vy'-1)

dibPelota :: Pelota -> Picture
dibPelota (Pelota x y vx vy) = Translate x y (Color red (Circle 10))

actualizar :: ViewPort -> Float -> Model -> Model
actualizar _ dt st = map (actPelota dt) st

dibujar :: Model -> Picture
dibujar st = Pictures (map dibPelota st)

main = do
    let ini = [Pelota (xx*50) (yy*50) (xx*15) (yy*15) | xx <- [-2..2], yy <- [-2..2]]
    simulate (FullScreen (1024,768)) white 60 ini dibujar actualizar
