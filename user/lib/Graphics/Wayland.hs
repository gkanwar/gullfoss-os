{- Hoo boy let's try something quite complex -}

module Graphics.Wayland where

foreign export ccall entry :: IO ()

entry :: IO ()
entry = putStrLn "hello world!"
