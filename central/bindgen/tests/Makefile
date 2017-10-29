all: test

include $(shell git rev-parse --show-toplevel)/../common.mk

$(call GEN_MEC_RULE)

CODE := $(wildcard *.me)
BYTECODE := $(patsubst %.me,%.mec,$(CODE))

build: $(BYTECODE)
test: build; $(call TEST_RUNNER_CMD,$(CURDIR))
clean:; -rm $(BYTECODE)

.PHONY: test
