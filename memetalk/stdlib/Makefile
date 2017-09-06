ROOT_DIR = $(shell pwd)/../..
MEME_STDLIB_DIR = $(ROOT_DIR)/memetalk/stdlib
PY_DIR = $(ROOT_DIR)/py

# compiling io using pycompiler
io.mmc: io.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_STDLIB_DIR)/io.mm

qt.mmc: qt.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_STDLIB_DIR)/qt.mm

all: io.mmc qt.mmc

clean:
	rm -f *.mmc
