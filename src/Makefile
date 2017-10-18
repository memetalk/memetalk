include $(shell git rev-parse --show-toplevel)/common.mk

BIN = $(MEME)

# General flags
INC_DIRS = /usr/include /usr/local/include

CXXFLAGS = --std=c++11 -Wall
LIBS = -ldl -lboost_system -lboost_iostreams -lboost_filesystem -lgc

CPP_FILES = log.cpp mec_image.cpp core_image.cpp vm.cpp utils.cpp	\
	mmobj.cpp main.cpp process.cpp prims.cpp mec_fun.cpp net_prims.cpp
HPP_FILES = mec_image.hpp core_image.hpp vm.hpp log.hpp utils.hpp	\
	defs.hpp mmobj.hpp process.hpp prims.hpp mec_fun.hpp net_prims.hpp


OBJS = $(CPP_FILES:%.cpp=%.o)

# Main targets
all: release
cleanvm: clean
clean:; rm -f $(BIN) $(OBJS)
build: $(BIN)

debug: CXXFLAGS += -g
debug: build

release: CXXFLAGS += -DMM_NO_DEBUG -O2 -g
release: build

# Output binary
%.o: %.cpp; g++ $(CXXFLAGS) -c -o $@ $^
$(BIN): $(OBJS); g++ -Wl,--export-dynamic $(OBJS) $(LIBS) -o $@
