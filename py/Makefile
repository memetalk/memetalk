subdirs = pyparsers

all:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) all;)

clean:
	$(foreach el,$(subdirs),$(MAKE) -C $(el) clean;)


parsers:
	$(MAKE) -C pyparsers all

.PHONY: $(subdirs) clean
