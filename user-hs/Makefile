BIN := bin
LIB := lib
GUEST := guest_os
GLAD := glad
APP := app

$(shell mkdir -p $(BIN)/app)
$(shell mkdir -p $(BIN)/glad)

.PHONY: all guest_os haskell_lib haskell_deps clean apps shell wallpaper

# NOTE: We assume a GHCup installation of 8.8.3 (I couldn't find nice ghc
# introspection flags...)
CXXFLAGS := -std=c++17 -O3 \
	-I$(HOME)/.ghcup/ghc/8.8.3/lib/ghc-8.8.3/include \
	-isystem $(GLAD)/include
LDFLAGS := -lpthread -lgmp -lz -ldl -lglfw #-framework OpenGL -liconv
GHCFLAGS := -O3 -package-db $(HOME)/.cabal/store/ghc-8.8.3/package.db

APP_SRCS := \
	shell.hs \
	wallpaper.hs
APP_OBJS := $(addprefix $(BIN)/app/,$(APP_SRCS:%.hs=%.o))
APPS := \
	shell \
	wallpaper
APP_OUTS := $(addprefix $(BIN)/app/,$(APPS))
LIB_DIR := 
LIB_SRCS := \
	Graphics/Wayland.hs
LIB_OBJS := $(addprefix $(BIN)/,$(LIB_SRCS:%.hs=%.o))
GUEST_SRCS := \
	graphics.cpp \
	input_events.cpp \
	main.cpp
GUEST_OBJS := $(addprefix $(BIN)/,$(GUEST_SRCS:%.cpp=%.o))
GLAD_SRCS := \
	glad.c
GLAD_OBJS := $(addprefix $(BIN)/glad/,$(GLAD_SRCS:%.c=%.o))


all: guest_os apps

# all dynamic runnables
$(BIN)/app/shell: $(BIN)/app/shell.o
	ghc $(EXTRA_GHCFLAGS) $(GHCFLAGS) $^ -outputdir $(BIN)/app -o $@
$(BIN)/app/wallpaper: $(BIN)/app/wallpaper.o
	ghc $(EXTRA_GHCFLAGS) $(GHCFLAGS) $^ -outputdir $(BIN)/app -o $@
apps: EXTRA_GHCFLAGS := $(shell cabal-cargs --cabalfile=$(APP)/app.cabal)
apps: $(APP_OUTS)

$(APP_OBJS): $(BIN)/app/%.o: $(APP)/%.hs
	ghc $(GHCFLAGS) $^ -outputdir $(BIN)/app -c -o $@
$(LIB_OBJS): $(BIN)/%.o: $(LIB)/%.hs
	ghc $(GHCFLAGS) $^ -threaded -outputdir $(BIN)

$(BIN)/hs_lib.a: $(LIB_OBJS)
	ghc $(GHCFLAGS) -no-hs-main -threaded -staticlib $^ -o $@

$(GLAD_OBJS): $(BIN)/glad/%.o: $(GLAD)/src/%.c
	g++ $(CXXFLAGS) -c $< -o $@
$(GUEST_OBJS): $(BIN)/%.o: $(GUEST)/%.cpp $(BIN)/hs_lib.a
	g++ $(CXXFLAGS) -I$(BIN)/ -c $< -o $@

haskell_deps:
	cabal build all --only-dependencies

$(BIN)/guest_os: EXTRA_GHCFLAGS := $(shell cabal-cargs --cabalfile=$(LIB)/lib.cabal)
$(BIN)/guest_os: $(GUEST_OBJS) $(GLAD_OBJS) $(BIN)/hs_lib.a
	ghc $(GHCFLAGS) $(EXTRA_GHCFLAGS) -no-hs-main -threaded $^ -lstdc++ $(LDFLAGS) -o $@
guest_os: $(BIN)/guest_os

clean:
	rm -r $(BIN)
