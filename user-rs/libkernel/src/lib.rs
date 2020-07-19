pub mod graphics;

// Interface to kernel "syscalls"

use cty::{c_char};

#[repr(C)]
#[derive(std::fmt::Debug)]
pub enum Signal {
  INT = 2
}

extern "C" {
  pub fn spawn(path: *const c_char) -> ();
}