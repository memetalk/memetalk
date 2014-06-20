sf-vm: clean
	g++ -Wall -g loader.cpp -o sf-vm
sf-vm: loader.hpp loader.cpp

clean:
	rm -f sf-vm

grammars: pyparsers/meme.g pyparsers/meme_tr.g pyparsers/core_tr.g
	python -m pyparsers.gen_parsers
