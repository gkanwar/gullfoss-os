BIN := bin
SRC := src
INCLUDE := include
KERNEL_OUT = $(BIN)/$(TARGET)/kernel.bin
PROJ_NAME := gullfoss

CROSSBIN := $(HOME)/opt/cross/bin
AS := $(CROSSBIN)/i686-elf-as
CC := $(CROSSBIN)/i686-elf-gcc
CXX := $(CROSSBIN)/i686-elf-g++
LD := $(CROSSBIN)/i686-elf-ld

.SUFFIXES: # remove default rules
.PHONY: debug release kernel clean kernel-binary grub-image


### Recursive/trivial make targets
ifndef TARGET

debug: export TARGET := debug
release: export TARGET := release
debug release:
	@$(MAKE)
clean:
	rm -rf $(BIN)

grub-image: TARGET := release
grub-image: release grub.cfg
	mkdir -p $(BIN)/iso/boot/grub
	cp $(KERNEL_OUT) $(BIN)/iso/boot/
	cp grub.cfg $(BIN)/iso/boot/grub/
	grub-mkrescue -o $(BIN)/$(TARGET)/$(PROJ_NAME).iso $(BIN)/iso


### Main make targets (TARGET is defined)
else

# make sure output dirs exist
TBIN := $(BIN)/$(TARGET)
$(shell mkdir -p $(TBIN)/kernel)
$(shell mkdir -p $(TBIN)/system)

CFLAGS := -ffreestanding -Wall -Wextra -Werror
CXXFLAGS := -std=c++17 -ffreestanding -fno-rtti -fno-exceptions -Wall -Wextra -Werror
ASFLAGS := -x assembler-with-cpp
RELEASE_FLAGS := -DNDEBUG -O3
DEBUG_FLAGS := -DDEBUG -O2 -g
ifeq ($(TARGET),release)
	CFLAGS := $(CFLAGS) $(RELEASE_FLAGS)
	CXXFLAGS := $(CXXFLAGS) $(RELEASE_FLAGS)
	ASFLAGS := $(ASFLAGS) $(RELEASE_FLAGS)
else
	CFLAGS := $(CFLAGS) $(DEBUG_FLAGS)
	CXXFLAGS := $(CXXFLAGS) $(DEBUG_FLAGS)
	ASFLAGS := $(ASFLAGS) $(DEBUG_FLAGS)
endif


KERNEL_SRCS = \
	bootstrap.s \
	debug_serial.cpp \
	heap_allocator.cpp \
	interrupt_impl.cpp \
	interrupt_manager.cpp \
	kernel.cpp \
	keyboard_state.cpp \
	memory.cpp \
	panic.cpp \
	phys_mem_allocator.cpp \
	pre_kernel.cpp \
	shell.cpp \
	vga.cpp \
	virt_mem_allocator.cpp
SYSTEM_SRCS = \
	stdlib.cpp

# identify all obj and dep files
KERNEL_OBJS := $(addprefix $(TBIN)/kernel/,$(patsubst %.cpp,%.o,$(KERNEL_SRCS:.s=.o)))
KERNEL_DEPS := $(KERNEL_OBJS:.o=.d)
SYSTEM_OBJS := $(addprefix $(TBIN)/system/,$(SYSTEM_SRCS:.cpp=.o))
SYSTEM_DEPS := $(SYSTEM_OBJS:.o=.d)

# list objs to compile and (ordered) link arg list for gcc
CRTBEGIN_OBJ := $(shell $(CC) $(CFLAGS) -print-file-name=crtbegin.o)
CRTEND_OBJ := $(shell $(CC) $(CFLAGS) -print-file-name=crtend.o)
OBJS = \
	$(TBIN)/kernel/crti.o \
	$(CRTBEGIN_OBJ) \
	$(KERNEL_OBJS) \
	$(SYSTEM_OBJS) \
	$(CRTEND_OBJ) \
	$(TBIN)/kernel/crtn.o
LIBS := -nostdlib -lgcc
LINK_LIST = \
	$(TBIN)/kernel/crti.o \
	$(CRTBEGIN_OBJ) \
	$(KERNEL_OBJS) \
	$(SYSTEM_OBJS) \
	$(LIBS) \
	$(CRTEND_OBJ) \
	$(TBIN)/kernel/crtn.o


# source rules
$(TBIN)/kernel/%.o: $(SRC)/kernel/%.cpp
	$(CXX) $(CXXFLAGS) -MD -MP -c $< -o $@ -isystem $(INCLUDE)/system -iquote $(INCLUDE)/kernel
$(TBIN)/kernel/%.o: $(SRC)/kernel/%.s
	$(CXX) $(ASFLAGS) -MD -MP -c $< -o $@ -isystem $(INCLUDE)/system -iquote $(INCLUDE)/kernel
$(TBIN)/system/%.o: $(SRC)/system/%.cpp
	$(CXX) $(CXXFLAGS) -MD -MP -c $< -o $@ -iquote $(INCLUDE)/system
# special rules
$(TBIN)/kernel/interrupt_impl.o: CXXFLAGS := $(CXXFLAGS) -mgeneral-regs-only

# build the kernel binary
$(KERNEL_OUT): $(OBJS) kernel.ld
	$(CXX) -T kernel.ld -o $@ $(CXXFLAGS) $(LINK_LIST)
kernel: $(KERNEL_OUT)


-include $(KERNEL_DEPS) $(SYSTEM_DEPS)

endif
