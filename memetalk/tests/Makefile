ROOT_DIR = $(shell pwd)/../..
#MEME_SCRIPT_DIR = $(shell pwd)
MEME_STDLIB_DIR = $(ROOT_DIR)/memetalk/stdlib
PY_DIR = $(ROOT_DIR)/py
TESTS_DIR = $(shell pwd)
MEME = $(ROOT_DIR)/meme



MMC_TESTS = $(TESTS_DIR)/*.mmc
MM_TESTS = $(TESTS_DIR)/*.mm
MEME_TESTER = $(MEME_STDLIB_DIR)/memetest.mm

all: $(MMC_TESTS)

clean:
	rm -f $(MMC_TESTS)

test: $(MMC_TESTS)
	$(MEME) $(MEME_TESTER)

$(MMC_TESTS): $(MM_TESTS)
	cd $(PY_DIR); python -m pycompiler.compiler $(MM_TESTS)

# MM_TESTS = $(MEME_DIR)/tests/*.mm
# MMC_TESTS = $(MEME_DIR)/tests/*.mmc

# TEST_DEPS = $(MEME_DIR)/stdlib/io.mmc
# MM_TESTER = $(MEME_DIR)/stdlib/memetest.mm
# MMC_TESTER = $(MEME_DIR)/stdlib/memetest.mmc

# # compile_tester: $(MMC_TESTER)
# # compile_all_tests: $(MMC_TESTS)

# $(TEST_DEPS): $(MEME_DIR)/stdlib/io.mm
# 	python -m pycompiler.compiler stdlib/io.mm

# $(MMC_TESTER): $(MM_TESTER) $(MEMETALK)
# 	python -m pycompiler.compiler $(MM_TESTER)


# $(MMC_TESTS): $(MM_TESTS) $(MEMETALK)
# 	python -m pycompiler.compiler $(MM_TESTS)

# stdlib/qt.mmc: stdlib/qt.mm # debugger tests need qt
# 	python -m pycompiler.compiler stdlib/qt.mm

# test: core.img $(TEST_DEPS) $(MMC_TESTER) $(MMC_TESTS) $(MEMETALK) stdlib/qt.mmc
# 	./sf-vm stdlib/memetest.mm
