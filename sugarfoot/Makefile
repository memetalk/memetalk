CPP_FILES =  mmc_image.cpp core_image.cpp vm.cpp report.cpp utils.cpp mmobj.cpp
HPP_FILES = mmc_image.hpp core_image.hpp vm.hpp report.hpp utils.hpp defs.hpp mmobj.hpp

sf-vm: clean
	g++ -Wall -g $(CPP_FILES) -o sf-vm

sf-vm: $(CPP_FILES) $(HPP_FILES)

clean:
	rm -f sf-vm

grammars: pyparsers/meme.g pyparsers/meme_tr.g pyparsers/core_tr.g
	python -m pyparsers.gen_parsers
