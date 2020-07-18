.PHONY: all apps guest_os
all: apps guest_os

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	DYLIB_EXT := so
else ifeq ($(UNAME_S),Darwin)
	DYLIB_EXT := dylib
endif
$(info DYLIB_EXT $(DYLIB_EXT))

APPS := wallpaper
APP_OUTS := $(foreach app,$(APPS),$(app)/target/debug/lib$(app).$(DYLIB_EXT))

define APP_TARGET
$(app)/target/debug/lib$(app).$(DYLIB_EXT): $(app)/src/* $(app)/Cargo.toml
	cd $(app) && cargo build
endef
$(foreach app,$(APPS),$(eval $(call APP_TARGET,$(app))))

apps: $(APP_OUTS)
guest_os:
	cd guest_os && $(MAKE)
