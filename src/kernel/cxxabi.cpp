#include "assert.h"

// Need for purely virtual functions
#define ATEXIT_FUNC_MAX 128
extern "C" {
  void __cxa_pure_virtual() { ASSERT_NOT_REACHED; }

  struct dtor_entry_t {
    void (*dtor)(void*) = nullptr;
    void* obj = nullptr;
    void* dso_handle = nullptr;
  };
  dtor_entry_t dtor_funcs[ATEXIT_FUNC_MAX];
  unsigned dtor_func_count = 0;
  unsigned __cxa_atexit(void (*dtor)(void*), void* obj, void* dso_handle) {
    if (dtor_func_count >= ATEXIT_FUNC_MAX) {
      return -1;
    }
    dtor_funcs[dtor_func_count++] = {
      .dtor = dtor, .obj = obj, .dso_handle = dso_handle
    };
    return 0;
  }

  void __cxa_finalize(void* f) {
    if (f == nullptr) {
      for (unsigned i = 0; i < dtor_func_count; ++i) {
        if (dtor_funcs[i].dtor) {
          (*dtor_funcs[i].dtor)(dtor_funcs[i].obj);
        }
      }
    }
    else {
      for (unsigned i = 0; i < dtor_func_count; ++i) {
        if (dtor_funcs[i].dtor == f) {
          (*dtor_funcs[i].dtor)(dtor_funcs[i].obj);
          dtor_funcs[i] = {0};
        }
      }
    }
  }
}
