/**
 * Very basic system clock tracking milliseconds since boot (since the PIT was
 * programmed, actually).
 */

// Low-resolution clock based on timer interrupts
class PITClock {
 public:
  PITClock();
  static PITClock& get();
  // Updates `ms_since_boot` based on the assumed constant PIT tick rate
  void tick();
  double get_ms() const;
 private:
  double ms_since_boot;
};
