import Control.Monad (when)
import Data.List (intersperse)

preguntarEdad :: String -> IO String
preguntarEdad nombre = do
    putStrLn ("Dame tu edad " ++ nombre)
    getLine

obtenerIMC :: IO Float
obtenerIMC = do
    putStrLn "Dame tu masa [kg]"
    peso <- getLine
    putStrLn "Dame tu altura [m]"
    altu <- getLine
    let imc = (read peso)/(read altu)^2
    return imc

retar :: IO ()
retar = do
    putStrLn "Dame tu edad"
    edad <- getLine
    if read edad < 12 then
        putStrLn "No deberías estar aquí!"
    else
        return ()

retar' :: IO ()
retar' = do
    putStrLn "Dame tu edad"
    edad <- getLine
    when (read edad < 8) (putStrLn "No deberías estar aquí!")

aceptarPorEdad :: IO Bool
aceptarPorEdad = do
    putStrLn "Dame tu edad"
    edad <- getLine
    if (read edad < 18) then do
        putStrLn "No deberías estar aquí!"
        return False
    else do
        putStrLn "Bienvenido a la dip web!"
        return True


preguntarEdades :: [String] -> IO [Int]
preguntarEdades names =
    let preguntar name = do
        putStrLn ("Qué edad tiene "++name++"?")
        edad <- getLine
        return (read edad)
    in mapM preguntar names

transform :: String -> String
transform input = let
    lins = lines input
    lins' = map reverse lins
    in concat (map (++"\n") lins')

main = interact transform
