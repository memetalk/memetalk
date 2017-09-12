all: build

subdirs = py src central

include common.mk

CORE_IMG = $(ROOT_DIR)/core.img

CORE_ME = $(MM_DIR)/stdlib/core.me

build: core
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

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

$(CORE_IMG):
	PYTHONPATH=$(PY_PATH) python -m pycore.compiler $(CORE_ME) $(ROOT_DIR)

.PHONY: $(subdirs) clean
