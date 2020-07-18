.PHONY: all guest_os
all: guest_os

BIN := bin
SRC := src
$(shell mkdir -p $(BIN))

CXXFLAGS := -std=c++17
LDFLAGS := -ldl

SRCS := main.cpp
OBJS := $(addprefix $(BIN)/,$(SRCS:%.cpp=%.o))

$(OBJS): $(BIN)/%.o: $(SRC)/%.cpp
	g++ $(CXXFLAGS) -c $^ -o $@

$(BIN)/guest_os: $(OBJS)
	g++ $(CXXFLAGS) $^ $(LDFLAGS) -o $@
guest_os: $(BIN)/guest_os
