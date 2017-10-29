all: build

include $(shell git rev-parse --show-toplevel)/../common.mk

$(call GEN_MEC_RULE)

CODE := net.me io.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))

DIST_FILES = $(CODE) $(BYTECODE)

build: $(BYTECODE)
clean:; -rm -f $(BYTECODE)
dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)
