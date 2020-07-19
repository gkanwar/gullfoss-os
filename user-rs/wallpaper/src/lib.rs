mod graphics;
use graphics::{Pixel};
use libc::{c_uint};
use std::{slice};

#[no_mangle]
pub extern "C" fn main(screen: *mut Pixel, width: c_uint, height: c_uint) -> ! {
  println!("Hello, wallpaper!");
  println!("Width = {width}, height = {height}", width=width, height=height);

  let screen = unsafe { slice::from_raw_parts_mut(screen, (width*height) as usize) };
  let mut lum = 0xff_u8;
  println!("Screen created!");
  loop {
    // Some random animation
    for pix in screen.iter_mut() {
      pix.r = lum;
      pix.g = lum;
      pix.b = lum;
    }
    lum = lum.checked_sub(1).unwrap_or(0xff);
  }
}
