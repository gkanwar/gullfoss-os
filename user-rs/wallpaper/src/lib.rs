extern crate kernel;
use kernel::{Signal};
use kernel::graphics;
use kernel::graphics::{Framebuffer};
use std::{slice};
use std::sync::atomic::{AtomicBool,Ordering};

static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub extern "C" fn main() -> () {
  // screen: *mut Pixel, width: c_uint, height: c_uint) -> ! {
  println!("Hello, wallpaper!");

  // let screen = unsafe { slice::from_raw_parts_mut(screen, (width*height) as usize) };
  let Framebuffer {pixels, width, height} = unsafe { graphics::get_framebuffer() };
  println!("Width = {width}, height = {height}", width=width, height=height);
  let screen = unsafe { slice::from_raw_parts_mut(pixels, (width*height) as usize) };
  let mut lum = 0xff_u8;
  // println!("Screen created!");
  while !SHOULD_QUIT.load(Ordering::Relaxed) {
    // Some random animation
    for pix in screen.iter_mut() {
      pix.r = lum;
      pix.g = lum;
      pix.b = lum;
    }
    lum = lum.checked_sub(1).unwrap_or(0xff);
  }
}

#[no_mangle]
pub extern "C" fn handle_signal(s: Signal) -> () {
  println!("Got signal: {:?}, quitting!", s);
  SHOULD_QUIT.store(true, Ordering::Relaxed);
}