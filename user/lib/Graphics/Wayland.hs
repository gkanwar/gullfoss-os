{- Hoo boy let's try something quite complex -}

module Graphics.Wayland where

import Codec.Picture.Repa (Img, RGBA, convertImage, imgData)
import Codec.Picture.Types (PixelRGBA8(..), PackeablePixel, Image, generateImage)
import Control.Concurrent as CC
import Data.Array.Repa (toUnboxed, computeUnboxedS)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Foreign
import Foreign.Ptr

-- Compositor interacts with clients and keeps an up-to-date committed screen
-- buffer that we can render from.
compositorThread :: Img RGBA -> MVar (Img RGBA) -> IO ()
compositorThread screen out = do
  tryPutMVar out screen
  -- TODO: update screen contents
  CC.threadDelay 10000
  compositorThread screen out

foreign export ccall waylandGetScreen :: StablePtr (MVar (Img RGBA)) -> IO (Ptr Word8)
waylandGetScreen handle = do
  ptr <- deRefStablePtr handle
  screen <- takeMVar ptr
  S.unsafeWith (U.convert (toUnboxed (computeUnboxedS (imgData screen)))) return

blackImage :: Int -> Int -> Image PixelRGBA8
blackImage width height =
  let s = fromIntegral . min 0xff
  in generateImage (\x y-> PixelRGBA8 (s (x+y)) (s x) (s y) (s 0xff)) width height

-- Start the main compositor thread
foreign export ccall waylandStartThread :: Int -> Int -> IO (StablePtr (MVar (Img RGBA)))
waylandStartThread :: Int -> Int -> IO (StablePtr (MVar (Img RGBA)))
waylandStartThread width height = do
  out <- newEmptyMVar
  let screen = (convertImage (blackImage width height)) :: Img RGBA in do
    CC.forkIO (compositorThread screen out)
    -- TODO: finalizer?
    ptr <- newStablePtr out
    putStrLn "threads started!"
    return ptr
