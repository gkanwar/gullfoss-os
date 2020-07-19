pub mod graphics;

// Interface to kernel "syscalls"

use cty::{size_t};

#[repr(C)]
#[derive(std::fmt::Debug)]
pub enum Signal {
  INT = 2
}

extern "C" {
  pub fn spawn(path: *const u8, path_len: size_t) -> ();
}