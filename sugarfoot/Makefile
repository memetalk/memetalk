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
LIBS = -lboost_system -lboost_iostreams -lboost_filesystem $(QT_LIBS)

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

release: CXXFLAGS += -DMM_NO_DEBUG -O2
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

cleanall: memeclean clean
	rm -f core.img
	rm -f $(PY_PARSER_FILES)
	rm -f pyparsers/*pyc
