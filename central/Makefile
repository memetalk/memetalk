subdirs = std linux reflection re2 kaiser ometa memescript

all:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

clean:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) clean;)

dist:
	$(foreach el,$(filter-out tests,$(subdirs)),$(MAKE) -C $(el) dist;)

debug:

.PHONY: $(subdirs) clean
