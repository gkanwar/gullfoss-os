#include <cstring>
#include "assert.h"
#include "virt_file_sys.h"

static VirtFileSystem* inst;
VirtFileSystem::VirtFileSystem() { assert_make_inst(inst, this); }
VirtFileSystem& VirtFileSystem::get() { return assert_get_inst(inst); }

void VirtFileSystem::mount(const char* path, unique_ptr<FileSystem> fs) {
  Mount mount = {.path = path, .fs = std::move(fs)};
  mounts.push_front(std::move(mount));
}

static bool is_prefix(const char* pre, const char* str) {
  return std::strncmp(pre, str, std::strlen(pre)) == 0;
}

unique_ptr<File> VirtFileSystem::open(const char* path) {
  for (auto&& mount : mounts) {
    const char* mount_prefix = mount.path;
    if (is_prefix(mount_prefix, path)) {
      const char* rel_path = &path[strlen(mount_prefix)];
      return mount.fs->open(rel_path);
    }
  }
  return nullptr;
}

void VirtFileSystem::close(unique_ptr<File>) {}


unique_ptr<File> InitrdFileSystem::open(const char* path) {
  tar_file_t file = initrd.find_file(path);
  if (!file.buffer)
    return nullptr;
  else
    return make_unique<File>(file.buffer);
}

void InitrdFileSystem::close(unique_ptr<File>) {}
