#ifndef ASSERT_H
#define ASSERT_H

#define MACRO_TO_STR_IN(x) #x
#define MACRO_TO_STR(x) MACRO_TO_STR_IN(x)
#define FILE_CONTEXT __FILE__ ":" MACRO_TO_STR(__LINE__)
#define PANIC_NOT_IMPLEMENTED(s) panic(FILE_CONTEXT "\nNot implemented: " #s)
#define ASSERT_NOT_REACHED panic(FILE_CONTEXT "\nShould not be reached")

#ifdef NDEBUG
#  define assert(x,s)
#else
#  define assert(x,s) if (!(x)) { panic(FILE_CONTEXT "\nAssert '" #x "' failed\nReason " #s); }
#endif


// "Pretty"-print our panic message and halt
[[noreturn]] void panic(const char* msg);

// Generic singleton constuctor and fetcher
template <typename T>
void assert_make_inst(T* &inst, T* _this) {
  assert(!inst, "Cannot construct multiple singleton instances");
  inst = _this;
}
template <typename T>
T& assert_get_inst(T* inst) {
  assert(inst, "Cannot get unconstructed singleton");
  return *inst;
}


#endif
