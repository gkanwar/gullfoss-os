BIN := bin
LIB := lib
GUEST := guest_os
APP := app

$(shell mkdir -p $(BIN)/app)

.PHONY: all guest_os haskell_lib clean

CXXFLAGS := -std=c++17 -O3 -I/usr/local/lib/ghc/include
LDFLAGS := -liconv -lglfw -framework OpenGL

APP_SRCS := \
	shell.hs
APP_OUTS := $(addprefix $(BIN)/app/,$(APP_SRCS:%.hs=%))
LIB_SRCS := \
	Graphics/Wayland.hs
LIB_OBJS := $(addprefix $(BIN)/,$(LIB_SRCS:%.hs=%.o))
GUEST_SRCS := \
	main.cpp
GUEST_OBJS := $(addprefix $(BIN)/,$(GUEST_SRCS:%.cpp=%.o))
$(info GUEST_OBJS $(GUEST_OBJS))


all: $(APP_OUTS) guest_os

$(APP_OUTS): $(BIN)/app/%: $(APP)/%.hs
	ghc $^ -outputdir $(BIN)/app -o $@
$(LIB_OBJS): $(BIN)/%.o: $(LIB)/%.hs
	ghc $^ -outputdir $(BIN)

$(BIN)/hs_lib.a: $(LIB_OBJS)
	ghc -no-hs-main -staticlib $^ -o $@

$(GUEST_OBJS): $(BIN)/%.o: $(GUEST)/%.cpp $(BIN)/hs_lib.a
	g++ $(CXXFLAGS) -I$(BIN)/ -c $< -o $@

$(BIN)/guest_os: $(GUEST_OBJS) $(BIN)/hs_lib.a
	g++ $(CXXFLAGS) -I$(BIN)/ $^ $(LDFLAGS) -o $@
guest_os: $(BIN)/guest_os

clean:
	rm -r $(BIN)
