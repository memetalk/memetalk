all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_MEC_RULE)

CODE = $(wildcard *.me)
BYTECODE := $(patsubst %.me,%.mec,$(CODE))
MEME_TESTER = $(ROOT_DIR)/central/stdlib/memetest.me

build: $(BYTECODE)
test: build; $(MEME) $(MEME_TESTER)
clean:; -rm -f $(BYTECODE)
