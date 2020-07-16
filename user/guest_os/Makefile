# Specific to OSX for now
BIN := bin
$(shell mkdir -p $(BIN))

.PHONY: guest_os clean

CXXFLAGS := -std=c++17 -O3
LDFLAGS := -lglfw -framework OpenGL

SRCS := \
	main.cpp
OBJS := $(addprefix $(BIN)/,$(SRCS:%.cpp=%.o))
$(BIN)/%.o: %.cpp
	g++ $(CXXFLAGS) -c $< -o $@

$(BIN)/guest_os: $(OBJS)
	g++ $(CXXFLAGS) $^ $(LDFLAGS) -o $@
guest_os: $(BIN)/guest_os

clean:
	rm -r $(BIN)
