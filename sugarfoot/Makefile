sf-vm: clean
	g++ -Wall -g report.cpp vm.cpp core_loader.cpp -o sf-vm
sf-vm: core_loader.hpp core_loader.cpp vm.cpp vm.hpp defs.hpp report.cpp report.hpp

clean:
	rm -f sf-vm

grammars: pyparsers/meme.g pyparsers/meme_tr.g pyparsers/core_tr.g
	python -m pyparsers.gen_parsers
