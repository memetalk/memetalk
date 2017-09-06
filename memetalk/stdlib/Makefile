ROOT_DIR = $(shell pwd)/../..
MEME_STDLIB_DIR = $(ROOT_DIR)/memetalk/stdlib
PY_DIR = $(ROOT_DIR)/py

# compiling io using pycompiler
io.mec: io.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_STDLIB_DIR)/io.me

qt.mec: qt.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_STDLIB_DIR)/qt.me

all: io.mec qt.mec

clean:
	rm -f *.mec
