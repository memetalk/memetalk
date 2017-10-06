subdirs = stdlib kaiser ometa memescript tests

all:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

clean:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) clean;)

test:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) test;)

debug:

.PHONY: $(subdirs) clean
