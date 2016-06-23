subdirs = sugarfoot

all: $(subdirs)

$(subdirs):; $(MAKE) -C $@

.PHONY: $(subdirs)
