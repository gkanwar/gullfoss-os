-- Sketch of a terminal user-space application

newtype Token = Token String
  deriving (Eq,Show)

tokenize :: String -> [Token]
tokenize s = map Token (words s) -- FORNOW

-- TODO: Use state monad to pass around an environ
-- resolveVars :: Environ -> Token -> Token

handleCmd :: [Token] -> IO Bool
handleCmd [] = return False
handleCmd ((Token "exit"):args) = return True
handleCmd ((Token s):args) = do
  putStrLn ("unrecognized cmd " ++ s)
  return False

repl :: IO ()
repl = do
  x <- getLine
  let toks = tokenize x in do
    exit <- handleCmd toks
    if not exit then repl else return ()

main :: IO ()
main = repl
