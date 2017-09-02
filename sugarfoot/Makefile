# Entry points:
#
#   -sf-vm [default]: builds the VM
#   -clean: deletes the VM binary and mmc files
#
#   -core: builds the core.img file
#   -memetalk: build the compiler, VM and core image
#   -test: runs all memetalk tests
#

############ VM Section ############

BIN = sf-vm

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
	mmobj.cpp main.cpp process.cpp prims.cpp mmc_fun.cpp		\
	remote_repl.cpp
HPP_FILES = mmc_image.hpp core_image.hpp vm.hpp log.hpp utils.hpp	\
	defs.hpp mmobj.hpp process.hpp prims.hpp mmc_fun.hpp		\
	remote_repl.hpp

CPP_FILES += qt_prims.cpp qsc.cpp ctrl.cpp
HPP_FILES += qt_prims.hpp qsc.hpp ctrl.hpp

OBJS = $(CPP_FILES:%.cpp=%.o)

# Main targets
all: release
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

############ Compiler Section ############

GRAMMARS = pyparsers/*.g
PY_PARSER_FILES = pyparsers/memetr.py pyparsers/parser.py
PY_COMPILER_DEPENDENCIES = $(GRAMMARS) pycompiler/*py pycore/*.py pyutils/*.py
MM_CORE = mm/core.mm
MEMETALK = core.img $(PY_PARSER_FILES) sf-vm

core: core.img
memetalk: $(MEMETALK)

$(PY_PARSER_FILES): $(GRAMMARS)
	python -m pyparsers.gen_parsers

core.img: $(PY_PARSER_FILES) $(MM_CORE) $(PY_COMPILER_DEPENDENCIES)
	python -m pycore.compiler

############ Memetalk Tests Section ############



MM_TESTS = tests/*.mm
MMC_TESTS = tests/*.mmc

TEST_DEPS = mm/io.mmc
MM_TESTER = mm/memetest.mm
MMC_TESTER = mm/memetest.mmc

# compile_tester: $(MMC_TESTER)
# compile_all_tests: $(MMC_TESTS)

$(TEST_DEPS): mm/io.mm
	python -m pycompiler.compiler mm/io.mm

$(MMC_TESTER): $(MM_TESTER) $(MEMETALK)
	python -m pycompiler.compiler $(MM_TESTER)


$(MMC_TESTS): $(MM_TESTS) $(MEMETALK)
	python -m pycompiler.compiler $(MM_TESTS)

mm/qt.mmc: mm/qt.mm # debugger tests need qt
	python -m pycompiler.compiler mm/qt.mm

test: core.img $(TEST_DEPS) $(MMC_TESTER) $(MMC_TESTS) $(MEMETALK) mm/qt.mmc
	MEME_PATH=./mm/ ./sf-vm mm/memetest.mmc


memeclean:
	rm -f mm/*.mmc
	rm -f tests/*.mmc
	rm -f compiler/*.mmc

cleanall: memeclean clean
	rm -f core.img
	rm -f $(PY_PARSER_FILES)
	rm -f pyparsers/*pyc

#################

reboot_ometa: compiler/ometa.g compiler/ometa_tr.g compiler/ometa_base.mmc compiler/ometa.mmc compiler/ometa_tr.mmc compiler/gen_parser.mmc mm/io.mmc
	./sf-vm gen_parser ometa
	./sf-vm gen_parser ometa_tr
	python -m pycompiler.compiler compiler/ometa.mm
	python -m pycompiler.compiler compiler/ometa_tr.mm

compiler/gen_parser.mmc: compiler/gen_parser.mm
	python -m pycompiler.compiler compiler/gen_parser.mm

compiler/meme.mm: compiler/meme.g compiler/ometa.mmc compiler/ometa_tr.mmc compiler/gen_parser.mmc
	./sf-vm gen_parser meme

compiler/meme_tr.mm: compiler/meme_tr.g compiler/ometa.mmc compiler/ometa_tr.mmc compiler/gen_parser.mmc
	./sf-vm gen_parser meme_tr

#

compiler/bits.mmc: compiler/bits.mm
	python -m pycompiler.compiler compiler/bits.mm

compiler/comp_vmemory.mmc: compiler/comp_vmemory.mm
	python -m pycompiler.compiler compiler/comp_vmemory.mm

compiler/vmemory.mmc: compiler/vmemory.mm
	python -m pycompiler.compiler compiler/vmemory.mm

compiler/opcode.mmc: compiler/opcode.mm
	python -m pycompiler.compiler compiler/opcode.mm

compiler/mmc.mmc: compiler/mmc.mm
	python -m pycompiler.compiler compiler/mmc.mm

compiler/entries.mmc: compiler/entries.mm
	python -m pycompiler.compiler compiler/entries.mm

compiler/ometa_base.mmc: compiler/ometa_base.mm
	python -m pycompiler.compiler compiler/ometa_base.mm

compiler/ometa.mmc: compiler/ometa.mm
	python -m pycompiler.compiler compiler/ometa.mm

compiler/ometa_tr.mmc: compiler/ometa_tr.mm
	python -m pycompiler.compiler compiler/ometa_tr.mm

compiler/meme.mmc: compiler/meme.mm
	python -m pycompiler.compiler compiler/meme.mm

compiler/meme_tr.mmc: compiler/meme_tr.mm
	python -m pycompiler.compiler compiler/meme_tr.mm

mm/io.mmc: mm/io.mm
	python -m pycompiler.compiler mm/io.mm

mm/remote_repl.mmc: mm/remote_repl.mm
	python -m pycompiler.compiler mm/remote_repl.mm

compiler/compiler.mmc: compiler/compiler.mm
	python -m pycompiler.compiler compiler/compiler.mm


mcompiler: compiler/bits.mmc compiler/comp_vmemory.mmc compiler/vmemory.mmc compiler/opcode.mmc compiler/mmc.mmc \
	compiler/entries.mmc compiler/ometa_base.mmc compiler/meme.mmc mm/io.mmc compiler/compiler.mmc mm/remote_repl.mmc \
	compiler/meme_tr.mmc compiler/ometa.mmc
