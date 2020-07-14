BIN := bin

$(shell mkdir -p $(BIN))

.PHONY: all clean

SRCS := \
	shell.hs
OUTPUTS := $(addprefix $(BIN)/,$(patsubst %.hs,%,$(SRCS)))

all: $(OUTPUTS)

$(BIN)/%: %.hs
	ghc $^ -outputdir $(BIN) -o $@

clean:
	rm -r $(BIN)
