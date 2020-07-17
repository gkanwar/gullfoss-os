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
compositorThread :: S.Vector Word8 -> MVar (S.Vector Word8) -> IO ()
compositorThread pixels out = do
  tryPutMVar out pixels
  -- TODO: update pixels contents
  CC.threadDelay 5000
  compositorThread pixels out

foreign export ccall waylandGetScreen :: StablePtr (MVar (S.Vector Word8)) -> IO (Ptr Word8)
waylandGetScreen handle = do
  ptr <- deRefStablePtr handle
  pixels <- takeMVar ptr
  S.unsafeWith pixels return

blackImage :: Int -> Int -> Image PixelRGBA8
blackImage width height =
  let s = fromIntegral . min 0xff
  in generateImage (\x y-> PixelRGBA8 (s (x+y)) (s x) (s y) (s 0xff)) width height

-- Start the main compositor thread
foreign export ccall waylandStartThread :: Int -> Int -> IO (StablePtr (MVar (S.Vector Word8)))
waylandStartThread :: Int -> Int -> IO (StablePtr (MVar (S.Vector Word8)))
waylandStartThread width height = do
  out <- newEmptyMVar
  let screen = (convertImage (blackImage width height)) :: Img RGBA
      pixels = (U.convert (toUnboxed (computeUnboxedS (imgData screen)))) :: S.Vector Word8
    in do
    CC.forkIO (compositorThread pixels out)
    -- TODO: finalizer?
    ptr <- newStablePtr out
    putStrLn "threads started!"
    return ptr
