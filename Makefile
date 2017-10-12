all: build

subdirs = py src central

include common.mk

CORE_ME = $(MM_DIR)/stdlib/core.me
CORE_IMG = $(ROOT_DIR)/core.img
DIST_FILES = $(CORE_IMG) etc.meme.config

VERSION = $(shell git describe --tags --always --dirty)

export DIST_DIR_NAME=memetalk-$(VERSION)

export DIST_DIR=$(ROOT_DIR)/$(DIST_DIR_NAME)

build: core
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

dist: build
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

test: build; $(MAKE) -C $(MM_DIR)/tests $@

src: core

core: $(CORE_IMG)

cleanall:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) clean;)
	rm -f core.img

$(CORE_IMG): $(MM_DIR)/stdlib/core.me
	$(MAKE) -C py parsers
	PYTHONPATH=$(PY_PATH) python -m pycore.compiler $(CORE_ME) $(ROOT_DIR)

.PHONY: $(subdirs) clean
