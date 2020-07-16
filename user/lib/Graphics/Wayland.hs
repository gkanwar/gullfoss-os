{- Hoo boy let's try something quite complex -}

module Graphics.Wayland where

import qualified Data.Vector.Unboxed as V

-- Just a quick test
foreign export ccall entry :: IO ()
entry :: IO ()
entry = putStrLn "hello world!"
