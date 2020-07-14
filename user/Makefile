BIN := bin

.PHONY: all

SRCS := \
	terminal.hs
OUTPUTS := $(addprefix $(BIN)/,$(patsubst %.hs,%,$(SRCS)))

all: $(OUTPUTS)

$(BIN)/%: %.hs
	ghc $^ -outputdir $(BIN) -o $@

