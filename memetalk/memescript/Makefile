ROOT_DIR = $(shell pwd)/../..
MEME_SCRIPT_DIR = $(shell pwd)
MEME_STDLIB_DIR = $(ROOT_DIR)/stdlib
PY_DIR = $(ROOT_DIR)/py

MEME = $(ROOT_DIR)/meme

all: memescript_compiler memescript_compiler


# compiling memescript compiler using pycompiler
bits.mec: bits.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/bits.me

comp_vmemory.mec: comp_vmemory.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/comp_vmemory.me

meme.mec: meme.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/meme.me

mec.mec: mec.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/mec.me

opcode.mec: opcode.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/opcode.me

compiler.mec: compiler.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/compiler.me

entries.mec: entries.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/entries.me

meme_tr.mec: meme_tr.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/meme_tr.me

ometa_base.mec: ometa_base.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/ometa_base.me

vmemory.mec: vmemory.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/vmemory.me

gen_parser.mec: gen_parser.me
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/gen_parser.me

memescript_compiler: bits.mec comp_vmemory.mec meme.mec mec.mec opcode.mec compiler.mec entries.mec meme_tr.mec ometa_base.mec vmemory.mec


# grammars
ometa.me: ometa.g
	$(MEME) ./gen_parser.me ometa

ometa_tr.me: ometa_tr.g
	$(MEME) ./gen_parser.me ometa_tr

meme.me: meme.g
	$(MEME) ./gen_parser.me meme

meme_tr.me: meme_tr.g
	$(MEME) ./gen_parser.me meme_tr

clean:
	rm -f *.mec
