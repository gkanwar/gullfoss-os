extern crate kernel;
extern crate image;
use kernel::{Signal};
use kernel::graphics;
use kernel::graphics::{Framebuffer};
use std::{slice};
use std::cmp::{min};
use std::sync::atomic::{AtomicBool,Ordering};
use image::{RgbaImage,Rgba};

static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);

fn load_default_wallpaper(width: u32, height: u32) -> RgbaImage {
  let res = image::open("resources/wallpaper.jpg");
  match res {
    Err(_) => {
      // display something pretty by default
      let img = RgbaImage::from_fn(width, height, |x, y| {
        Rgba([ min(x+y,0xff) as u8, min(x,0xff) as u8,
               min(y,0xff) as u8, 0xff_u8])
      });
      return img;
    }
    Ok(img) => {
      // TODO: do rescaling, cropping, etc. to get a (width,height) img
      return img.to_rgba();
    }
  }
}

#[no_mangle]
pub extern "C" fn main() -> () {
  println!("Hello, wallpaper!");

  let Framebuffer {pixels, width, height} = unsafe { graphics::get_framebuffer() };
  println!("Width = {width}, height = {height}", width=width, height=height);
  let wallpaper = load_default_wallpaper(width, height);
  let screen = unsafe { slice::from_raw_parts_mut(pixels, (width*height) as usize) };
  for (pix, wall_pix) in screen.iter_mut().zip(wallpaper.pixels()) {
    pix.r = wall_pix[0];
    pix.g = wall_pix[1];
    pix.b = wall_pix[2];
    pix.a = wall_pix[3];
  }
  while !SHOULD_QUIT.load(Ordering::Relaxed) {
    std::thread::yield_now();
  }
}

#[no_mangle]
pub extern "C" fn handle_signal(s: Signal) -> () {
  println!("Got signal: {:?}, quitting!", s);
  SHOULD_QUIT.store(true, Ordering::Relaxed);
}