# Entry points:
#
#   -sf-vm [default]: builds the VM
#   -clean: deletes the VM binary and mmc files
#
#   -core: builds the core.img file
#   -memetalk: build the VM and core image
#   -test: runs all memetalk tests
#

############ VM Section ############
ROOT_DIR = $(shell pwd)/..
PY_DIR = $(ROOT_DIR)/py
MEME_DIR = $(ROOT_DIR)/memetalk

BIN = $(ROOT_DIR)/meme

# General flags
INC_DIRS = /usr/include /usr/local/include

# QT Flags
INC_QSCI ?= $(dir $(foreach i,$(INC_DIRS),$(shell find -L $(i) -name qscicommandset.h)))
QT_PKGLIBS = QtCore QtGui QtScript QtWebKit QtNetwork
QT_CXXFLAGS = $(shell pkg-config --cflags $(QT_PKGLIBS)) -I$(INC_QSCI)
QT_LIBS = $(shell pkg-config --libs $(QT_PKGLIBS)) -lqscintilla2

CXXFLAGS = -Wall $(QT_CXXFLAGS)
LIBS = -lboost_system -lboost_iostreams -lboost_filesystem -lgc $(QT_LIBS)

CPP_FILES = log.cpp mmc_image.cpp core_image.cpp vm.cpp utils.cpp	\
	mmobj.cpp main.cpp process.cpp prims.cpp mmc_fun.cpp
HPP_FILES = mmc_image.hpp core_image.hpp vm.hpp log.hpp utils.hpp	\
	defs.hpp mmobj.hpp process.hpp prims.hpp mmc_fun.hpp

CPP_FILES += qt_prims.cpp qsc.cpp ctrl.cpp
HPP_FILES += qt_prims.hpp qsc.hpp ctrl.hpp

OBJS = $(CPP_FILES:%.cpp=%.o)

# Main targets
all: release core
clean:; rm -f $(BIN) $(OBJS) qsc.moc.cpp
build: $(BIN)

debug: CXXFLAGS += -g
debug: build

release: CXXFLAGS += -DMM_NO_DEBUG -O2 -g
release: build

# Qsc file
qsc.moc.cpp: qsc.hpp; moc $< > $@

# Output binary
%.o: %.cpp; g++ $(CXXFLAGS) -c -o $@ $^
$(BIN): qsc.moc.cpp $(OBJS); g++ $(OBJS) $(LIBS) -o $@

############ end VM Section ############

############ Core Section ############
CORE_IMG = $(ROOT_DIR)/core.img
MM_CORE = $(MEME_DIR)/stdlib/core.mm

$(CORE_IMG): $(MM_CORE)
	cd $(PY_DIR); python -m pycore.compiler $(MM_CORE) $(ROOT_DIR)

core: $(CORE_IMG)

############ Memetalk Tests Section ############

# memeclean:
# 	rm -f stdlib/*.mmc
# 	rm -f tests/*.mmc
# 	rm -f compiler/*.mmc

# cleanall: memeclean clean
# 	rm -f core.img
# 	rm -f $(PY_PARSER_FILES)
# 	rm -f pyparsers/*pyc


# reboot_ometa: memescript/ometa.g memescript/ometa_tr.g memescript/ometa_base.mmc memescript/ometa.mmc memescript/ometa_tr.mmc memescript/gen_parser.mmc stdlib/io.mmc
# 	./sf-vm gen_parser ometa
# 	./sf-vm gen_parser ometa_tr
# 	python -m pycompiler.compiler memescript/ometa.mm
# 	python -m pycompiler.compiler memescript/ometa_tr.mm

# memescript/gen_parser.mmc: memescript/gen_parser.mm
# 	python -m pycompiler.compiler memescript/gen_parser.mm

# memescript/meme.mm: memescript/meme.g memescript/ometa.mmc memescript/ometa_tr.mmc memescript/gen_parser.mmc
# 	./sf-vm gen_parser meme

# memescript/meme_tr.mm: memescript/meme_tr.g memescript/ometa.mmc memescript/ometa_tr.mmc memescript/gen_parser.mmc
# 	./sf-vm gen_parser meme_tr

# #

# memescript/bits.mmc: memescript/bits.mm
# 	python -m pycompiler.compiler memescript/bits.mm

# memescript/comp_vmemory.mmc: memescript/comp_vmemory.mm
# 	python -m pycompiler.compiler memescript/comp_vmemory.mm

# memescript/vmemory.mmc: memescript/vmemory.mm
# 	python -m pycompiler.compiler memescript/vmemory.mm

# memescript/opcode.mmc: memescript/opcode.mm
# 	python -m pycompiler.compiler memescript/opcode.mm

# memescript/mmc.mmc: memescript/mmc.mm
# 	python -m pycompiler.compiler memescript/mmc.mm

# memescript/entries.mmc: memescript/entries.mm
# 	python -m pycompiler.compiler memescript/entries.mm

# memescript/ometa_base.mmc: memescript/ometa_base.mm
# 	python -m pycompiler.compiler memescript/ometa_base.mm

# memescript/ometa.mmc: memescript/ometa.mm
# 	python -m pycompiler.compiler memescript/ometa.mm

# memescript/ometa_tr.mmc: memescript/ometa_tr.mm
# 	python -m pycompiler.compiler memescript/ometa_tr.mm

# memescript/meme.mmc: memescript/meme.mm
# 	python -m pycompiler.compiler memescript/meme.mm

# memescript/meme_tr.mmc: memescript/meme_tr.mm
# 	python -m pycompiler.compiler memescript/meme_tr.mm

# stdlib/io.mmc: stdlib/io.mm
# 	python -m pycompiler.compiler stdlib/io.mm

# stdlib/remote_repl.mmc: stdlib/remote_repl.mm
# 	python -m pycompiler.compiler stdlib/remote_repl.mm

# memescript/compiler.mmc: memescript/compiler.mm
# 	python -m pycompiler.compiler memescript/compiler.mm


# mcompiler: memescript/bits.mmc memescript/comp_vmemory.mmc memescript/vmemory.mmc memescript/opcode.mmc memescript/mmc.mmc \
# 	memescript/entries.mmc memescript/ometa_base.mmc memescript/meme.mmc stdlib/io.mmc memescript/compiler.mmc stdlib/remote_repl.mmc \
# 	memescript/meme_tr.mmc memescript/ometa.mmc
