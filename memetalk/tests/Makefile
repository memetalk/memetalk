ROOT_DIR = $(shell pwd)/../..
#MEME_SCRIPT_DIR = $(shell pwd)
MEME_STDLIB_DIR = $(ROOT_DIR)/memetalk/stdlib
PY_DIR = $(ROOT_DIR)/py
TESTS_DIR = $(shell pwd)
MEME = $(ROOT_DIR)/meme



MEC_TESTS = $(TESTS_DIR)/*.mec
MM_TESTS = $(TESTS_DIR)/*.me
MEME_TESTER = $(MEME_STDLIB_DIR)/memetest.me

all: $(MEC_TESTS)

clean:
	rm -f $(MEC_TESTS)

test: $(MEC_TESTS)
	$(MEME) $(MEME_TESTER)

$(MEC_TESTS): $(MM_TESTS)
	cd $(PY_DIR); python -m pycompiler.compiler $(MM_TESTS)
