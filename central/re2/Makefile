all: build

include $(shell git rev-parse --show-toplevel)/../common.mk

$(call GEN_MEC_RULE)

## meme

CODE := re2.me
BYTECODE := $(patsubst %.me,%.mec,$(CODE))

DIST_FILES = $(CODE) $(BYTECODE) meme_re2.so

dist: $(DIST_FILES); $(call INSTALL_DIST_FILES,$^)

### src

INC_DIRS = $(ROOT_DIR)/src

CXXFLAGS = --std=c++11 -Wall -I$(INC_DIRS)
LIBS = -lre2

CPP_FILES = re2_prims.cpp
OBJS = $(CPP_FILES:%.cpp=%.o)

%.o: %.cpp; g++ $(CXXFLAGS) -fPIC -c -o $@ $^

meme_re2.so: $(OBJS)
	g++ -shared -fPIC $(LIBS) re2_prims.o -o meme_re2.so

clean:; -rm -f $(BYTECODE) $(OBJS) meme_re2.so

build: meme_re2.so $(BYTECODE)
