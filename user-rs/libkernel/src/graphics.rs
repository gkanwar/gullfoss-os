// Interface to graphics "driver"

use cty::{c_uint};

#[repr(C,packed)]
#[derive(std::clone::Clone)]
pub struct Pixel {
  pub r: u8,
  pub g: u8,
  pub b: u8,
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