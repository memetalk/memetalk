all: build

include $(shell git rev-parse --show-toplevel)/../common.mk

$(call GEN_OMETA_TR_RULE)
$(call GEN_MEC_RULE)
$(call GEN_KAISER_TR_RULE)

CODE := bits.me comp_vmemory.me core_vmemory.me mec.me opcode.me compiler.me	\
	entries.me vmemory.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))
DIST_FILES = $(CODE) meme.me meme_tr.me $(BYTECODE)

KAISER_GRAMMARS := meme.k

GRAMMARS := meme_tr.g
GCODE := $(patsubst %.g,%.me,$(GRAMMARS))
GBCODE := $(patsubst %.g,%.mec,$(GRAMMARS))
DIST_FILES += $(GRAMMARS) $(KAISER_GRAMMARS) $(GBCODE)

KCODE := $(patsubst %.k,%.me,$(KAISER_GRAMMARS))
KBCODE := $(patsubst %.k,%.mec,$(KAISER_GRAMMARS))
DIST_FILES += $(KBCODE)

build: $(BYTECODE) $(GCODE) $(GBCODE) $(KCODE) $(KBCODE)
clean:; -rm -f $(BYTECODE) $(GBCODE)
dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)
