ROOT_DIR = $(shell pwd)/../..
MEME_SCRIPT_DIR = $(shell pwd)
MEME_STDLIB_DIR = $(ROOT_DIR)/stdlib
PY_DIR = $(ROOT_DIR)/py

MEME = $(ROOT_DIR)/meme

all: memescript_compiler memescript_compiler


# compiling memescript compiler using pycompiler
bits.mmc: bits.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/bits.mm

comp_vmemory.mmc: comp_vmemory.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/comp_vmemory.mm

meme.mmc: meme.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/meme.mm

mmc.mmc: mmc.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/mmc.mm

opcode.mmc: opcode.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/opcode.mm

compiler.mmc: compiler.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/compiler.mm

entries.mmc: entries.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/entries.mm

meme_tr.mmc: meme_tr.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/meme_tr.mm

ometa_base.mmc: ometa_base.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/ometa_base.mm

vmemory.mmc: vmemory.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/vmemory.mm

gen_parser.mmc: gen_parser.mm
	cd $(PY_DIR); python -m pycompiler.compiler $(MEME_SCRIPT_DIR)/gen_parser.mm

memescript_compiler: bits.mmc comp_vmemory.mmc meme.mmc mmc.mmc opcode.mmc compiler.mmc entries.mmc meme_tr.mmc ometa_base.mmc vmemory.mmc


# grammars
ometa.mm: ometa.g
	$(MEME) ./gen_parser.mm ometa

ometa_tr.mm: ometa_tr.g
	$(MEME) ./gen_parser.mm ometa_tr

meme.mm: meme.g
	$(MEME) ./gen_parser.mm meme

meme_tr.mm: meme_tr.g
	$(MEME) ./gen_parser.mm meme_tr

clean:
	rm -f *.mmc
