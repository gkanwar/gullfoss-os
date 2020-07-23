#![feature(lang_items)]
#![no_std]

pub mod graphics;

// Interface to kernel "syscalls"

use cty::{size_t};

#[repr(C)]
#[derive(core::fmt::Debug)]
pub enum Signal {
  INT = 2
}

#[panic_handler]
pub fn panic(_panic: &core::panic::PanicInfo<'_>) -> ! {
  loop { unsafe { r#yield(); } }
}

#[lang = "eh_personality"]
extern "C" fn eh_personality() {
  // TODO: deal with unwinding the stack
}

#[link(name="kernel_stubs", kind="dylib")]
extern "C" {
  pub fn spawn(path: *const u8, path_len: size_t) -> ();
  pub fn r#yield() -> ();
  pub fn exit(code: u8) -> !;
}
