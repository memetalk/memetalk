subdirs = py src central

include common.mk

CORE_IMG = $(ROOT_DIR)/core.img

CORE_ME = $(MM_DIR)/stdlib/core.me

all:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

clean:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) clean;)

debug:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) debug;)

test:; $(MAKE) -C $(MM_DIR)/tests $@

src: core

core: $(CORE_IMG)

$(CORE_IMG):
	cd $(PY_DIR); python -m pycore.compiler $(CORE_ME) $(ROOT_DIR)

.PHONY: $(subdirs) clean
