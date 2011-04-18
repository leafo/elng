-module(native).
-compile(export_all).

print(X) when is_number(X) ->
	io:format("~p~n", [X]).

print_sum(X, Y) when is_number(X), is_number(Y) ->
	io:format("~p~n", [X + Y]).
