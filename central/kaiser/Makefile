all: build

include $(shell git rev-parse --show-toplevel)/../common.mk

$(call GEN_OMETA_TR_RULE)
$(call GEN_MEC_RULE)

CODE := kaiser_base.me gen_parser.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))
DIST_FILES = $(CODE) kaiser.me kaiser_tr.me $(BYTECODE)

OMETA_GRAMMARS := kaiser.g kaiser_tr.g

GCODE := $(patsubst %.g,%.me,$(OMETA_GRAMMARS))
GBCODE := $(patsubst %.g,%.mec,$(OMETA_GRAMMARS))
DIST_FILES += $(OMETA_GRAMMARS) $(GBCODE)

KCODE := $(patsubst %.k,%.me,$(KAISER_GRAMMARS))
KBCODE := $(patsubst %.k,%.mec,$(KAISER_GRAMMARS))
DIST_FILES += $(KBCODE)

build: $(BYTECODE) $(GCODE) $(GBCODE) $(KCODE) $(KBCODE)
clean:; -rm -f $(BYTECODE) $(GBCODE)
dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)
