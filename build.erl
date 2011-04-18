%!/usr/bin/escript
-module(build).
-export([main/1]).

main(["scanner", File]) ->
	Atom = list_to_atom(File),
	{ok, _} = leex:file(Atom);

main(["parser", File]) ->
	Atom = list_to_atom(File),
	{ok, _} = yecc:file(Atom).
