// Interface to graphics "driver"

#[repr(C,packed)]
#[derive(std::clone::Clone)]
pub struct Pixel {
  pub r: u8,
  pub g: u8,
  pub b: u8,
  pub a: u8,
}
