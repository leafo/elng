
all: scanner.beam parser.beam
	erl -make

scanner.beam: scanner.xrl
	escript build.erl scanner $<

parser.beam: parser.yrl
	escript build.erl parser $<

clean:
	-rm scanner.erl
	-rm parser.erl
	-rm *.beam
	