all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_MEC_RULE)

CODE := re2.me io.me qt.me remote_repl.me memetest.me

BYTECODE := $(patsubst %.me,%.mec,$(CODE))


build: $(BYTECODE)
clean:; -rm -f $(BYTECODE)
