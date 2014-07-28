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


LIB_PATHS = -L/usr/local/Cellar/boost/1.54.0/lib

CPP_FILES =  mmc_image.cpp core_image.cpp vm.cpp report.cpp utils.cpp mmobj.cpp main.cpp process.cpp prims.cpp

HPP_FILES = mmc_image.hpp core_image.hpp vm.hpp report.hpp utils.hpp defs.hpp mmobj.hpp process.hpp prims.hpp

LIBS = -lboost_system-mt -lboost_filesystem-mt

sf-vm: $(CPP_FILES) $(HPP_FILES)
	g++ -Wall -g $(CPP_FILES) $(LIBS) $(LIB_PATHS) -o sf-vm

clean:
	rm -f sf-vm
	rm mm/*.mmc
	rm tests/*.mmc

############ Compiler Section ############


GRAMMARS = pyparsers/*.g
PY_PARSER_FILES = pyparsers/memetr.py pyparsers/parser.py
PY_COMPILER_DEPENDENCIES = $(GRAMMARS) pycompiler/*py pycore/*.py pyutils/*.py
MM_CORE = mm/core.md
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

test: $(TEST_DEPS) $(MMC_TESTER) $(MMC_TESTS) $(MEMETALK)
	MEME_PATH=./mm/ ./sf-vm mm/memetest.mmc
