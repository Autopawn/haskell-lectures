-- Este programa lleva una lista de TODOs:
putTodo :: (Int, String) -> IO ()
putTodo (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
    putStrLn ""
    putStrLn "Current TODO list:"
    mapM_ putTodo (zip [0..] todos)
    command <- getLine
    interpret command todos

interpret :: String -> [String] -> IO ()
interpret ('+':' ':todo) todos = prompt (todo:todos)
interpret ('-':' ':num ) todos =
    case delete (read num) todos of
        Nothing -> do
            putStrLn "No TODO entry matches the given number"
            prompt todos
        Just todos' -> prompt todos'
interpret  "q"           todos = return ()
interpret  command       todos = do
    putStrLn ("Invalid command: `" ++ command ++ "`")
    prompt todos

delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    as' <- delete (n - 1) as
    return (a:as')
delete _  []    = Nothing

main = prompt []

{-
1. Explique cómo se utiliza este programa y la utilidad de cada función.

"prompt" imprime toda la lista y recibe el siguiente comando, interpret reconoce el comando y modifica la lista de todos, volviendo a llamar a prompt, delete elimina el comando de la lista con el entero marcado.

2. Si Ud. quisiera que un TODO fuera su propio datatype en vez de un (Int,String) ¿Cómo lo declararía? ¿Convendría instanciarlo en algunas typeclasses?

Se podría hacer data Todo = Todo Int String, se podría instanciar en Eq para eliminarlos con funciones más simples, el problema sería agregar Todos nuevos y que se mantenga el ítem.

3. ¿Qué restricciones existen en Haskell para las dependencias entre módulos?

No pueden existir dependencias cíclicas.

4. ¿Cuál es la diferencia entre map y mapM?

El segundo recibe una función que obtiene una monad de un valor resultante por cada valor entrante y los entrega como un foldable de Monads.

5. ¿Cuándo y para qué se utiliza return al final de un bloque do?

Para promover un resultado puro a una Monad y ser consistente con el tipo resultado que debe entregar.

6. Nombre una situación en que la lazyness genere un thunk que crezca, consumiendo memoria, de forma ilimitada.


-}
