ROOT_DIR = $(shell pwd)/..
PY_DIR = $(ROOT_DIR)/py
MEME_DIR = $(ROOT_DIR)/central

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

CPP_FILES = log.cpp mec_image.cpp core_image.cpp vm.cpp utils.cpp	\
	mmobj.cpp main.cpp process.cpp prims.cpp mec_fun.cpp
HPP_FILES = mec_image.hpp core_image.hpp vm.hpp log.hpp utils.hpp	\
	defs.hpp mmobj.hpp process.hpp prims.hpp mec_fun.hpp

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
CORE_ME = $(MEME_DIR)/stdlib/core.me

$(CORE_IMG): $(CORE_ME)
	cd $(PY_DIR); python -m pycore.compiler $(CORE_ME) $(ROOT_DIR)

core: $(CORE_IMG)
