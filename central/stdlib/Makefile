subdirs = bindgen

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_MEC_RULE)

CODE := io.me qt.me remote_repl.me memetest.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))

all: build; $(foreach el,$(subdirs),$(MAKE) -C $(el) all)

test: build; $(foreach el,$(subdirs),$(MAKE) -C $(el) test)

clean:; -rm -f $(BYTECODE); $(foreach el,$(subdirs),$(MAKE) -C $(el) clean)

build: $(BYTECODE)

.PHONY: test
