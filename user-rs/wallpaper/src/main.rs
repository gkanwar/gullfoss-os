#![no_std]
#![no_main]

extern crate kernel;
// extern crate image;
use kernel::{Signal};
use kernel::graphics::*;
use core::{slice};
// use core::cmp::{min};
use core::sync::atomic::{AtomicBool,Ordering};
// use image::{RgbaImage,Rgba};

static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);

// fn load_default_wallpaper(width: u32, height: u32) -> RgbaImage {
//   let res = image::open("resources/wallpaper.jpg");
//   match res {
//     Err(_) => {
//       // display something pretty by default
//       let img = RgbaImage::from_fn(width, height, |x, y| {
//         Rgba([ min(x+y,0xff) as u8, min(x,0xff) as u8,
//                min(y,0xff) as u8, 0xff_u8])
//       });
//       return img;
//     }
//     Ok(img) => {
//       // TODO: do rescaling, cropping, etc. to get a (width,height) img
//       return img.to_rgba();
//     }
//   }
// }

#[derive(Copy,Clone)]
struct Complex {
  x: f32,
  y: f32
}
// TODO: should use traits etc
fn complex_normsq(z: Complex) -> f32 {
  z.x*z.x + z.y*z.y
}
fn complex_add(z1: Complex, z2: Complex) -> Complex {
  Complex {
    x: z1.x + z2.x,
    y: z1.y + z2.y
  }
}
fn complex_mul(z1: Complex, z2: Complex) -> Complex {
  Complex {
    x: z1.x * z2.x - z1.y * z2.y,
    y: z1.x * z2.y + z1.y * z2.x
  }
}
fn complex_powu(z: Complex, u: u8) -> Complex {
  let mut pow_z = z;
  for _i in 1..u {
    pow_z = complex_mul(pow_z, z)
  }
  return pow_z;
}

fn compute_mandelbrot(width: u32, height: u32, screen: &mut [Pixel]) -> () {
  // convert coord to local in range [a,b]
  fn get_local(coord: u32, max: u32, a: f32, b: f32) -> f32 {
    return a + (b-a) * (coord as f32) / (max as f32);
  }
  // get mandelbrot iters to diverging past radius 2, or -1 if not diverged
  fn get_iter_count(c: Complex) -> i32 {
    let mut z = c;
    for i in 0..32 {
      if complex_normsq(z) >= 4.0 {
        return i;
      }
      z = complex_add(complex_powu(z, 2), c);
    }
    return -1;
  }
  let ax = -3.0;
  let bx = 1.0;
  let ay = -2.0*(height as f32)/(width as f32);
  let by = 2.0*(height as f32)/(width as f32);
  for (i, pix) in screen.iter_mut().enumerate() {
    let x = get_local((i as u32) % width, width, ax, bx);
    let y = get_local((i as u32) / width, height, ay, by);
    let mut c = get_iter_count(Complex{x: x, y: y});
    if c == -1 {
      c = 0xff;
    }
    else {
      c = c*4;
    }
    let gray: u8 = c as u8;
    pix.b = gray;
    pix.g = gray;
    pix.r = gray;
  }
}

#[no_mangle]
pub extern "C" fn _start() -> ! {
  // println!("Hello, wallpaper!");

  let mut c = Complex {x: 1.0, y: 3.0};
  // IPC to compositor
  unsafe {
    kernel::send(31337, (&mut c as *mut Complex) as *mut u8);
  }

  let Framebuffer {pixels, width, height} = unsafe { get_framebuffer() };
  // println!("Width = {width}, height = {height}", width=width, height=height);
  // let wallpaper = load_default_wallpaper(width, height);
  let screen = unsafe { slice::from_raw_parts_mut(pixels, (width*height) as usize) };
  // for (pix, wall_pix) in screen.iter_mut().zip(wallpaper.pixels()) {
  //   pix.r = wall_pix[0];
  //   pix.g = wall_pix[1];
  //   pix.b = wall_pix[2];
  //   pix.a = wall_pix[3];
  // }
  // simple gray wallpaper
  for pix in screen.iter_mut() {
    pix.r = 0x08;
    pix.g = 0x08;
    pix.b = 0x08;
    pix.a = 0xff;
  }
  compute_mandelbrot(width, height, screen);
  while !SHOULD_QUIT.load(Ordering::Relaxed) {
    unsafe { kernel::r#yield(); }
    // std::thread::yield_now();
  }
  unsafe { kernel::exit(0); }
}

// TODO: need to register signal handlers instead of looking for magic names
#[no_mangle]
pub extern "C" fn handle_signal(_s: Signal) -> () {
  // println!("Got signal: {:?}, quitting!", s);
  SHOULD_QUIT.store(true, Ordering::Relaxed);
}
