# Important paths

## Root Directory of the entire projec
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOT_DIR := $(strip $(patsubst %/, %, $(dir $(mkfile_path))))


## Them MEME!!!
MEME = $(ROOT_DIR)/meme

## Directory for the virtual machine
VM_DIR := $(ROOT_DIR)/src

## Path to the .mm files required for the VM
MM_DIR := $(ROOT_DIR)/central

## All paths with relevant MemeTalk code
MM_PATH := $(MM_DIR)

LOCAL_CONFIG := $(ROOT_DIR)/meme.config

## Path to the VM command
VM_CMD := MEME_CONFIG=$(LOCAL_CONFIG) $(MEME)

COMPILER_CMD := $(VM_CMD) -c
## Macro GEN_MEC_RULE: Creates Make rule to compile .me files
##
##   This macro comes in handy for directories full of .me source
##   files that must be compiled into .mec bytecode. A Makefile for
##   directories like that will like like this:
##
##     all: build
##     include $(shell git rev-parse --show-toplevel)/common.mk
##
##     CODE := source1.me source2.me source3.me
##     BYTECODE := $(subst .me,.mec,$(CODE))
##
##     $(call GEN_MEC_RULE)
##     build: $(BYTECODE)
##     clean:; -rm $(BYTECODE)
##
define GEN_MEC_RULE
 %.mec: %.me; $(COMPILER_CMD) $$^
endef

## Create implicit rules for building .g and .k into .me
##
##   Use this macro whenever you got grammar files to be translated into
##   MemeTalk files. This is supposed to be the only way the compiler
##   gets executed. If we ever need to change parameters that affect
##   everyone, here's the ideal place.
##

define GEN_OMETA_TR_RULE
 %.me: %.g; $(call RUN_VM_CMD,$(MM_DIR)/ometa/gen_parser.me $$(CURDIR)/$$^)
endef

define GEN_KAISER_TR_RULE
 %.me: %.k; $(call RUN_VM_CMD,$(MM_DIR)/kaiser/gen_parser.me $$(CURDIR)/$$^)
endef

## Macro TEST_RUNNER_CMD: Outputs the command to call the test runner
##
##   Output the command for running tests on a directory. The only
##   parameter it takes is `path`. It accepts absolute paths that
##   contain compiled test files.
##
##   Usage:
##
##     # (...) assuming you have the rules to compile .me into .mec
##     build: $(BYTECODE)
##     test: build; $(call TEST_RUNNER_CMD,$(PWD))
##
define TEST_RUNNER_CMD
 $(call RUN_VM_CMD,$(MM_DIR)/std/memetest.me $(1))
endef

define RUN_BINDGEN_CMD
 $(call RUN_VM_CMD,$(MM_DIR)/bindgen/bindgen.me $(1))
endef

## Macro RUN_VM_CMD: Outputs the command to run the VM
##
##   We currently have to run the binary of the Virtual Machine from
##   the same directory as the `core.img`. This macro wraps up the
##   whole command line for doing that so logic for building paths and
##   setting environment variables is centralized in one place.
##
##   Here's how it can be used in a Makefile:
##
##     bindgen_tr.me: bindgen_tr.g; $(call RUN_VM_CMD,$(PWD)/gen.mec)
##
define RUN_VM_CMD
 $(VM_CMD) $(1)
endef

## Macro INSTALL_DIST_FILES: Copy files into package directory
define INSTALL_DIST_FILES
 install -D -m 644 -t $(DIST_DIR)/$(subst $(ROOT_DIR),,$(CURDIR)) $(1)
endef
