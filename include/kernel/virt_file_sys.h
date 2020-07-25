#ifndef VIRT_FILE_SYS_H
#define VIRT_FILE_SYS_H

#include <forward_list>
#include <memory>
#include "tar.h"
#include "types.h"

using namespace std;

// TODO: generalize beyond an in-memory file
class File {
 public:
  File(const u8* buffer) : buffer(buffer) {}
  const u8* buffer;
};

class FileSystem {
 public:
  virtual unique_ptr<File> open(const char* path) = 0; // TODO: open flags
  virtual void close(unique_ptr<File> fd) = 0;
};

class VirtFileSystem : public FileSystem {
 public:
  VirtFileSystem();
  static VirtFileSystem& get();
  void mount(const char* path, unique_ptr<FileSystem> fs);
  unique_ptr<File> open(const char* path) override; // TODO: open flags
  void close(unique_ptr<File> fd) override;
 private:
  struct Mount {
    const char* path;
    unique_ptr<FileSystem> fs;
  };
  forward_list<Mount> mounts;
};

class InitrdFileSystem : public FileSystem {
 public:
  InitrdFileSystem(Tarball initrd) : initrd(initrd) {}
  unique_ptr<File> open(const char* path) override;
  void close(unique_ptr<File> fd) override;
 private:
  Tarball initrd;
};


#endif
