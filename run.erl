-module(run).
-compile(export_all).

read(Name) when is_list(Name) ->
	case file:read_file(Name) of
		{ok, Bin} -> binary_to_list(Bin);
		_ -> {error, "Failed to find file " ++ Name}
	end;
read(Name) when is_atom(Name) ->
	read(atom_to_list(Name) ++ ".leaf").

tokens(Name) ->
	File = read(Name),
	{ok, Tokens, _} = calc_scanner:string(File),
	Tokens.

tree(Name) ->
	{ok, Tree} = calc_parser:parse(tokens(Name)),
	Tree.

file(Name) ->
	block(tree(Name)).

prepare_env() ->
	Env = env:new(),
	env:set("print", {native_func, native, print}, Env),
	env:set("print_sum", {native_func, native, print_sum}, Env),
	Env.

block(Ops) -> block(Ops, prepare_env()).

block([Op|[]], Env) ->
	stm(Op, Env);
block([Op|Rest], Env) ->
	stm(Op, Env),
	block(Rest, Env).

zip_set([], _, Env) -> Env;
zip_set(_, [], Env) -> Env;
zip_set([Name|Names], [Value|Values], Env) ->
	env:set(Name, Value, Env),
	zip_set(Names, Values, Env).

call(Name, Args, Env) ->
	case env:get(Name, Env) of
		{ok, {native_func, Module, Func}} -> {ok, apply(Module, Func, Args)};
		{ok, {func, ArgNames, Code}} ->
			FuncEnv = zip_set(ArgNames, [eval(A, Env) || A <- Args], env:new(Env)),
			{ok, block(Code, FuncEnv)};
		_ -> error
	end.

% run a statement
stm({'let', Id, Exp}, Env) ->
	Val = eval(Exp, Env),
	env:set(Id, Val, Env),
	Val;

stm({fundef, Id, ArgNames, Code}, Env) ->
	env:set(Id, {func, ArgNames, Code}, Env);

stm({funcall, Id, Args}, Env) ->
	% eval the args
	EvalArgs = [eval(A, Env) || A <- Args],
	case call(Id, EvalArgs, Env) of
		{ok, Value} -> Value;
		error -> io:format("Failed to call function ~p~n", [Id]), error
	end.

eval({add, L, R}, Env) -> eval(L, Env) + eval(R, Env);
eval({sub, L, R}, Env) -> eval(L, Env) - eval(R, Env);
eval({mul, L, R}, Env) -> eval(L, Env) * eval(R, Env);
eval({'div', L, R}, Env) -> eval(L, Env) div eval(R, Env);
eval({deref, Id}, Env) -> 
	case env:get(Id, Env) of
		{ok, Value} -> Value;
		error -> io:format("Referenced undeclared variable ~p~n", [Id]), error
	end;
eval(X, _) when is_number(X) -> X.

