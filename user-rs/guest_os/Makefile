.PHONY: all guest_os clean
all: guest_os

BIN := bin
SRC := src
$(shell mkdir -p $(BIN))

CXXFLAGS := -std=c++17
LDFLAGS := -rdynamic -ldl -lglfw -lpthread

# system-specific
UNAME_S = $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	CXXFLAGS += -isystem ../glad/include
else ifeq ($(UNAME_S),Darwin)
	LDFLAGS += -framework OpenGL
endif


HDRS := $(wildcard $(SRC)/*.h) # TODO: gcc deps
SRCS := main.cpp \
	graphics.cpp \
	input_events.cpp \
	process.cpp
OBJS := $(addprefix $(BIN)/,$(SRCS:%.cpp=%.o))
LINK_OBJS := $(OBJS) ../glad/bin/glad.o

$(OBJS): $(BIN)/%.o: $(SRC)/%.cpp
	g++ $(CXXFLAGS) -c $^ -o $@

$(BIN)/guest_os: $(OBJS)
	g++ $(CXXFLAGS) $(LINK_OBJS) $(LDFLAGS) -o $@
guest_os: $(BIN)/guest_os

clean:
	rm -r $(BIN)
