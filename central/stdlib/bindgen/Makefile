all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_TRS_RULE)
$(call GEN_MEC_RULE)

CODE := bindgen.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))

GRAMMARS := definitions_pr.g definitions_tr.g
GCODE := $(patsubst %.g,%.me,$(GRAMMARS))
GBCODE := $(patsubst %.g,%.mec,$(GRAMMARS))

build: $(BYTECODE) $(GCODE) $(GBCODE)
clean:; -rm $(BYTECODE) $(GCODE) $(GBCODE)
test: build; $(MAKE) -C $(CURDIR)/tests

.PHONY: test
