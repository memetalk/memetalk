subdirs = sugarfoot

all: $(subdirs)

$(subdirs):; $(MAKE) -C $@

clean:; $(foreach el,$(subdirs),$(MAKE) -C $(el) clean)

.PHONY: $(subdirs) clean
