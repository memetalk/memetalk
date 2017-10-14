all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_MEC_RULE)

CODE = $(wildcard *.me)
BYTECODE := $(patsubst %.me,%.mec,$(CODE))
MEME_TESTER = $(ROOT_DIR)/central/std/memetest.me

build: $(BYTECODE)
test: build; $(VM_CMD) $(MEME_TESTER)
clean:; -rm -f $(BYTECODE)
