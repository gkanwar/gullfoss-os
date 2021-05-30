#include "pci.h"

#include "assert.h"

constexpr u16 CONFIG_ADDRESS = 0xCF8;
constexpr u16 CONFIG_DATA = 0xCFC;

u16 read_config_u16(u32 bus, u32 device, u32 func, u8 offset) {
  u32 enable = 1 << 31;
  assert(offset & 1 == 0, "Offset must be word-aligned");
  bool word_off = offset & 0b10;
  offset &= 0b11111100;
  u32 address = enable | (bus << 16) | (device << 11) | (func << 8) | offset;
  out32(CONFIG_ADDRESS, address);
  return (u16) ((in32(CONFIG_DATA) >> (16*word_off)) & 0xffff);
}
