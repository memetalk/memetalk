subdirs = std linux kaiser ometa memescript bindgen tests

all:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

clean:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) clean;)

test:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) test;)

dist:
	$(foreach el,$(filter-out tests,$(subdirs)),$(MAKE) -C $(el) dist;)

debug:

.PHONY: $(subdirs) clean
