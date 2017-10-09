all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_OMETA_TR_RULE)
$(call GEN_MEC_RULE)

CODE := bits.me comp_vmemory.me core_vmemory.me meme.me mec.me opcode.me compiler.me	\
	entries.me vmemory.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))
DIST_FILES = $(BYTECODE)

GRAMMARS := meme.g meme_tr.g
GCODE := $(patsubst %.g,%.me,$(GRAMMARS))
GBCODE := $(patsubst %.g,%.mec,$(GRAMMARS))
DIST_FILES += $(GBCODE)

build: $(BYTECODE) $(GCODE) $(GBCODE)
clean:; -rm -f $(BYTECODE) $(GBCODE)
dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)
