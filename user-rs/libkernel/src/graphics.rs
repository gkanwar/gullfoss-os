// Interface to graphics "driver"

use cty::{c_uint};

#[repr(C,packed)]
#[derive(Copy,Clone)]
pub struct Pixel {
  // TODO: on OSX seems to be BGRA instead, is it the same on Linux?
  // Need to fix convention for graphics driver.
  pub b: u8,
  pub g: u8,
  pub r: u8,
  pub a: u8,
}

#[repr(C,packed)]
pub struct Framebuffer {
  pub pixels: *mut Pixel,
  pub width: c_uint,
  pub height: c_uint,
}

extern "C" {
  pub fn get_framebuffer() -> Framebuffer;
}
