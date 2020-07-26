#![no_std]
#![no_main]

extern crate kernel;
use kernel::{Signal};
// use kernel::graphics;
// use kernel::graphics::{Framebuffer};
// use core::{slice};
use core::sync::atomic::{AtomicBool,Ordering};

static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub extern "C" fn _start() -> ! {
  // println!("Hello, compositor!");

  let app_name = "/apps/wallpaper";
  unsafe {
    kernel::spawn(app_name.as_ptr(), app_name.len());
  }

  // receive IPC
  unsafe {
    let ptr = kernel::accept(31337);
  }

  // let Framebuffer {pixels, width, height} = unsafe { graphics::get_framebuffer() };
  // println!("Width = {width}, height = {height}", width=width, height=height);
  // let screen = unsafe { slice::from_raw_parts_mut(pixels, (width*height) as usize) };
  // let mut lum = 0xff_u8;
  // println!("Screen created!");
  while !SHOULD_QUIT.load(Ordering::Relaxed) {
    // Some random animation
    // for pix in screen.iter_mut() {
    //   pix.r = lum;
    //   pix.g = lum;
    //   pix.b = lum;
    // }
    // lum = lum.checked_sub(1).unwrap_or(0xff);
    // std::thread::yield_now();
    unsafe { kernel::r#yield(); }
  }
  unsafe { kernel::exit(0); }
}

// TODO: need to register signal handlers instead of looking for magic names
#[no_mangle]
pub extern "C" fn handle_signal(_s: Signal) -> () {
  // println!("Got signal: {:?}, quitting!", s);
  SHOULD_QUIT.store(true, Ordering::Relaxed);
}
