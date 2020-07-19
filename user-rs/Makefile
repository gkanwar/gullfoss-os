.PHONY: all apps guest_os glad clean
all: apps guest_os

BIN := bin
CARGO_TARGET := app_target
$(shell mkdir -p $(BIN)/apps)

# host-specific variables
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	DYLIB_EXT := so
else ifeq ($(UNAME_S),Darwin)
	DYLIB_EXT := dylib
endif
$(info DYLIB_EXT $(DYLIB_EXT))

# per-app targets
APPS := wallpaper
APP_OUTS := $(addprefix $(BIN)/apps/,$(APPS))
define APP_TARGET
$(CARGO_TARGET)/debug/lib$(app).$(DYLIB_EXT): $(app)/src/* $(app)/Cargo.toml
	cd $(app) && cargo build
$(BIN)/apps/$(app): $(CARGO_TARGET)/debug/lib$(app).$(DYLIB_EXT)
	cp $$^ $$@
endef
$(foreach app,$(APPS),$(eval $(call APP_TARGET,$(app))))

# all apps
apps: $(APP_OUTS)

# the guest OS
guest_os/$(BIN)/guest_os: glad
	cd guest_os && $(MAKE)
$(BIN)/guest_os: guest_os/$(BIN)/guest_os
	cp $^ $@
guest_os: $(BIN)/guest_os

# GLAD library
glad:
	cd glad && $(MAKE)

clean: 	# TODO: clean app dirs
	rm -rf $(BIN)
	rm -rf $(CARGO_TARGET)
	cd guest_os && $(MAKE) clean
