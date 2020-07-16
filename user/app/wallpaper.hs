{-# LANGUAGE TemplateHaskell #-}
-- TODO: load wallpaper resource using FFI

import Codec.Picture.Jpg (decodeJpeg)
import Codec.Picture.Types
  (DynamicImage(..), Image, PixelRGBA8(..), PixelRGB8(..),
   generateImage, convertImage, pixelMap)
import qualified Data.ByteString as DBS
import Data.FileEmbed
import qualified Data.Vector as V

import Debug.Trace
import System.IO.Unsafe

width = 800
height = 600

defaultWallpaper :: Image PixelRGBA8
defaultWallpaper =
  let jpg = decodeJpeg $(embedFile "app/wallpaper.jpg") in
  case jpg of
    Right (ImageYCbCr8 img) -> convertImage (pixelMap (
      \x -> case x of PixelRGB8 r g b -> PixelRGBA8 r g b (fromIntegral 0xff)
      ) ((convertImage img) :: Image PixelRGB8))
    _ -> simpleWallpaper

simpleWallpaper :: Image PixelRGBA8
simpleWallpaper =
  let s = fromIntegral . min 0xff
  in generateImage (\x y-> PixelRGBA8 (s (x+y)) (s x) (s y) (s 0xff)) width height

main :: IO ()
main = let wp = defaultWallpaper
  in wp `seq` putStrLn "Wallpaper loaded"
