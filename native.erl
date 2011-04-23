-module(native).
-compile(export_all).

print([]) -> io:format("\n", []);
print([X | []]) ->
	io:format("~p\n", [X]);
print([X | Rest]) ->
	io:format("~p, ", [X]),
	print(Rest).

print_sum([X, Y]) when is_number(X), is_number(Y) ->
	io:format("~p~n", [X + Y]).


