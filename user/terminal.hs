import Data.List

-- Sketch of a terminal user-space application

newtype Token = Token {str :: String}
  deriving (Eq,Show)

tokenize :: String -> [Token]
tokenize s = map Token (words s) -- FORNOW

-- TODO: Use state monad to pass around an environ
-- resolveVars :: Environ -> Token -> Token

echo :: [Token] -> IO ()
echo toks = putStrLn $ concat (intersperse " " (map str toks))

handleCmd :: [Token] -> IO Bool
handleCmd [] = return False
handleCmd ((Token "exit"):args) = return True
handleCmd ((Token "echo"):args) = do
  echo args
  return False
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
