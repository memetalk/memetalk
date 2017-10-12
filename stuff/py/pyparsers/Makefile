all: parser.py memetr.py

parser.py memetr.py : gen_parsers.py meme.g meme_tr.g astbuilder.py
	python gen_parsers.py

clean:
	rm -f parser.py memetr.py *.pyc
