.PHONY: all clean objs
all: objs

BIN := bin
SRC := src
$(shell mkdir -p $(BIN))

CFLAGS := -isystem include

SRCS := glad.c
OBJS := $(addprefix $(BIN)/,$(SRCS:%.c=%.o))

$(OBJS): $(BIN)/%.o: $(SRC)/%.c
	gcc $(CFLAGS) -c $< -o $@
objs: $(OBJS)

clean:
	rm -r $(BIN)
