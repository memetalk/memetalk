LIB_PATHS = -L/usr/local/Cellar/boost/1.54.0/lib

CPP_FILES =  mmc_image.cpp core_image.cpp vm.cpp report.cpp utils.cpp mmobj.cpp main.cpp process.cpp prims.cpp

HPP_FILES = mmc_image.hpp core_image.hpp vm.hpp report.hpp utils.hpp defs.hpp mmobj.hpp process.hpp prims.hpp

LIBS = -lboost_system-mt -lboost_filesystem-mt

sf-vm: clean
	g++ -Wall -g $(CPP_FILES) $(LIBS) $(LIB_PATHS) -o sf-vm

sf-vm: $(CPP_FILES) $(HPP_FILES)

clean:
	rm -f sf-vm

grammars: pyparsers/meme.g pyparsers/meme_tr.g pyparsers/core_tr.g
	python -m pyparsers.gen_parsers
