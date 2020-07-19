// Interface to kernel "syscalls"

use cty::{c_char};
use super::graphics::{Framebuffer};

#[repr(C)]
#[derive(std::fmt::Debug)]
pub enum Signal {
  INT = 2
}

#[allow(dead_code)]
extern "C" {
  pub fn spawn(path: *const c_char) -> ();
  pub fn get_framebuffer() -> Framebuffer;
}