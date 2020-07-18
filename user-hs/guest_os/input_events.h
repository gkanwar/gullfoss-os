/**
 * Mock input events driver. Translates GLFW events to standard input events and
 * broadcasts them to root graphical program. (TODO: events arch)
 */

#pragma once

class InputEvents {
 public:
  InputEvents();
  static InputEvents& get();
};
