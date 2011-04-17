
all: calc_scanner.beam calc_parser.beam

calc_scanner.beam: calc_scanner.xrl
	escript build.erl scanner $<

calc_parser.beam: calc_parser.yrl
	escript build.erl parser $<

clean:
	-rm calc_scanner.erl
	-rm calc_parser.erl
	-rm *.beam
	