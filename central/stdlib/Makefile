subdirs = bindgen

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_MEC_RULE)

CODE := net.me re2.me io.me remote_repl.me memetest.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))

DIST_FILES = $(CODE) $(BYTECODE) core.me

build: $(BYTECODE)
clean:; -rm -f $(BYTECODE)
dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)
