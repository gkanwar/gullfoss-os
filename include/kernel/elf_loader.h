#ifndef ELF_LOADER_H
#define ELF_LOADER_H

#include <memory>
#include "block_source.h"

class ELFLoader {
 public:
  ELFLoader(UniquePtr<BlockSource>);
  // TODO: methods to load proc given a ProcAllocator
 private:
  UniquePtr<BlockSource> src;
};

#endif
