all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_MEC_RULE)

CODE := remote_repl.me memetest.me ext.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))

DIST_FILES = $(CODE) $(BYTECODE) core.me

build: core.img $(BYTECODE)
clean:; -rm -f $(BYTECODE)
dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)


core.img: core.me
	$(COMPILER_CMD) core.me
