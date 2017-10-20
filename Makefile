all: buildall

subdirs = src central tests

include common.mk

CORE_ME = $(MM_DIR)/std/core.me
CORE_IMG = $(ROOT_DIR)/core.img
DIST_FILES = $(CORE_IMG) etc.meme.config

VERSION = $(shell git describe --tags --always --dirty)

export DIST_DIR_NAME=memetalk-$(VERSION)

export DIST_DIR=$(ROOT_DIR)/$(DIST_DIR_NAME)

buildall:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

get_core:
	wget -O core.img http://modules.memetalk.org/central/std/core-0.0.1.img

dist: buildall
	mkdir -p $(DIST_DIR)
	$(MAKE) -C central dist
	$(call INSTALL_DIST_FILES,$(DIST_FILES))
	install -D -t $(DIST_DIR) meme
	install -D -t $(DIST_DIR) install.sh
	tar zcf $(DIST_DIR_NAME).tar.gz $(DIST_DIR_NAME)
	rm -r $(DIST_DIR)

clean:
	$(MAKE) -C src clean

debug:
	$(MAKE) -C src debug

test: buildall; $(MAKE) -C tests $@

src: core

core: $(CORE_IMG)

cleanall:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) clean;)
	rm -f core.img

$(CORE_IMG): $(MM_DIR)/std/core.me
	$(COMPILER_CMD) $(CORE_ME)

.PHONY: $(subdirs) clean
