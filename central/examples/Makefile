all: build

include $(shell git rev-parse --show-toplevel)/common.mk

$(call GEN_MEC_RULE)

CODE := hello_world.me  Makefile  ometa_example.me  \
	ometa_grammar_example.me  using_calc.me

BYTECODE := $(patsubst %.me,%.mec,$(CODE))


build: $(BYTECODE)
clean:; -rm -f $(BYTECODE)
