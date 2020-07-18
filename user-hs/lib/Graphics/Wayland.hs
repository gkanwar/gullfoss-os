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
type NativePixel = PT.PackedRepresentation P.PixelRGBA8
type NativeImage = PT.Image NativePixel

makePixel :: Int -> Int -> Int -> Int -> NativePixel
makePixel r g b a =
  let s = fromIntegral . min 0xff
  in PT.packPixel (P.PixelRGBA8 (s r) (s g) (s b) (s a))

-- Compositor interacts with clients and keeps an up-to-date committed screen
-- buffer that we can render from.
compositorThread :: NativeImage -> IO ()
compositorThread screen = do
  CC.threadDelay 5000
  nextScreen <- stToIO $ do
    mutScreen <- PT.unsafeThawImage screen
    P.writePixel mutScreen 50 50 (makePixel 0xff 0xff 0xff 0xff)
    P.writePixel mutScreen 50 51 (makePixel 0xff 0xff 0xff 0xff)
    PT.unsafeFreezeImage mutScreen
  compositorThread nextScreen

-- Start the main compositor thread
foreign export ccall waylandStart :: Int -> Int -> Ptr Word32 -> IO ()
waylandStart :: Int -> Int -> Ptr Word32 -> IO ()
waylandStart width height ptr = do
  fp <- (newForeignPtr_ ptr)
  let d = SM.unsafeFromForeignPtr0 fp (width*height)
      screen = PT.MutableImage width height d
      initScreen = runST $ do
        PT.fillImageWith screen (makePixel 0x08 0x08 0x08 0xff)
        PT.unsafeFreezeImage screen
  _ <- CC.forkIO (compositorThread initScreen)
  putStrLn "threads started!"

