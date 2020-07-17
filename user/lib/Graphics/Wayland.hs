{- Hoo boy let's try something quite complex -}

module Graphics.Wayland where

import qualified Codec.Picture as P
import qualified Codec.Picture.Types as PT
import Control.Concurrent as CC
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign

-- We natively work with RGBA8 images
type NativeImage s = PT.MutableImage s P.PixelRGBA8

makePixel :: Int -> Int -> Int -> Int -> P.PixelRGBA8
makePixel r g b a =
  let s = fromIntegral . min 0xff
  in P.PixelRGBA8 (s r) (s g) (s b) (s a)

-- Compositor interacts with clients and keeps an up-to-date committed screen
-- buffer that we can render from.
compositorThread :: Int -> Int -> Ptr Word32 -> IO ()
compositorThread width height ptr = do
  fp <- (newForeignPtr_ ptr)
  let d = SM.unsafeFromForeignPtr0 fp (width*height)
      screen = PT.MutableImage width height d in do
    stToIO $ P.writePixel screen 50 50 (PT.packPixel (makePixel 0xff 0xff 0xff 0xff))
    stToIO $ P.writePixel screen 50 51 (PT.packPixel (makePixel 0xff 0xff 0xff 0xff))
  CC.threadDelay 5000
  compositorThread width height ptr

-- Start the main compositor thread
foreign export ccall waylandStart :: Int -> Int -> Ptr Word32 -> IO ()
waylandStart :: Int -> Int -> Ptr Word32 -> IO ()
waylandStart width height ptr = do
  fp <- (newForeignPtr_ ptr)
  let d = SM.unsafeFromForeignPtr0 fp (width*height)
      screen = PT.MutableImage width height d in
    stToIO $ PT.fillImageWith screen (PT.packPixel (makePixel 0x08 0x08 0x08 0xff))
  _ <- CC.forkIO (compositorThread width height ptr)
  putStrLn "threads started!"

