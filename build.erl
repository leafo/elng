#!/usr/bin/escript


compile(File) ->
	compile:file(File, []).

main(["scanner", File]) ->
	Atom = list_to_atom(File),
	{ok, _} = leex:file(Atom),
	compile(Atom);

main(["parser", File]) ->
	Atom = list_to_atom(File),
	{ok, _} = yecc:file(Atom),
	compile(Atom).
