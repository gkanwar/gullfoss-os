#include "elf_loader.h"

ELFLoader::ELFLoader(UniquePtr<BlockSource> src) :
    src(std::move(src)) {}

