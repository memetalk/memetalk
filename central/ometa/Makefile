all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_TRS_RULE)
$(call GEN_MEC_RULE)

CODE := ometa.me ometa_base.me ometa_tr.me gen_parser.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))
DIST_FILES = $(BYTECODE)

GRAMMARS := ometa.g ometa_tr.g
GCODE := $(patsubst %.g,%.me,$(GRAMMARS))
GBCODE := $(patsubst %.g,%.mec,$(GRAMMARS))
DIST_FILES += $(GBCODE)

build: $(BYTECODE) $(GCODE) $(GBCODE)
clean:; -rm -f $(BYTECODE) $(GBCODE)
dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)
