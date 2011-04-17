-module(run).
-compile(export_all).

file(Name) when is_list(Name) ->
	{ok, Bin} = file:read_file(Name);
	binary_to_list(Bin);

file(Name) when is_atom(Name) ->
	file(atom_to_list(Name) ++ ".leaf").

