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
	{ok, Tokens, _} = scanner:string(File),
	Tokens.

tree(Name) ->
	{ok, Tree} = parser:parse(tokens(Name)),
	Tree.

file(Name) ->
	block(tree(Name)).

string(Code) ->
	{ok, Tokens, _} = scanner:string(Code),
	{ok, Tree} = parser:parse(Tokens),
	block(Tree).


%%
%% Interpreter
%%

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

bool(true) -> 1;
bool(_) -> 0.

call(Name, Args, Env) ->
	case env:get(Name, Env) of
		{ok, {native_func, Module, Func}} -> {ok, apply(Module, Func, [Args])};
		{ok, {func, ArgNames, Code}} ->
			FuncEnv = zip_set(ArgNames, [eval(A, Env) || A <- Args], env:new(Env)),
			{ok, catch block(Code, FuncEnv)};
		_ -> error
	end.

for_loop(Id, Value, End, Inc, Code, Env) ->
	InnerEnv = env:new(Env),
	V = eval(Value, Env),
	env:set(Id, V, InnerEnv),
	Ret = block(Code, InnerEnv),
	if 
		V== End -> Ret;
		true -> for_loop(Id, V+Inc, End, Inc, Code, Env)
	end.

declare_list([{Id, Exp} | Rest], Env) ->
	Val = eval(Exp, Env),
	env:set(Id, Val, Env),
	declare_list(Rest, Env);
declare_list([], _) -> ok.

% run a statement
stm({'let', Id, Exp}, Env) ->
	Val = eval(Exp, Env),
	env:set(Id, Val, Env),
	{return, Val};

stm({'let', LetList}, Env) ->
	declare_list(LetList, Env);

stm({fundef, Id, ArgNames, Code}, Env) ->
	env:set(Id, {func, ArgNames, Code}, Env),
	ok;

stm({funcall, Id, Args}, Env) ->
	EvalArgs = [eval(A, Env) || A <- Args],
	case call(Id, EvalArgs, Env) of
		{ok, Value} -> Value;
		error -> io:format("Failed to call function ~p~n", [Id]), error
	end;

stm({return, Exp}, Env) -> throw({return, eval(Exp, Env)});
stm({return}, _) -> throw({return});

stm({'if', Cond, Code, ElseCode}, Env) ->
	case eval(Cond, Env) of
		0 -> case ElseCode of
				none -> ok;
				_ -> block(ElseCode, Env)
			end;
		_ -> block(Code, Env)
	end;

stm({'for', Id, Min, Max, Code}, Env) ->
	Inc = if Min < Max -> 1; true -> -1 end,
	for_loop(Id, Min, Max, Inc, Code, Env);

stm({'while', Cond, Code}, Env) ->
	V = eval(Cond, Env),
	case V of
		1 ->
			block(Code, Env),
			stm({while, Cond, Code}, Env);
		_ -> ok
	end;

stm(Other, Env) -> {return, eval(Other, Env)}.

% evaluate expression, expected to return value or error
eval(FunCall, Env) when element(1, FunCall) == funcall ->
	case stm(FunCall, Env) of
		{return, Value} -> Value;
		_ -> io:format("Expecting return value from function~n", []), error
	end;

eval({assign, Name, Exp}, Env) ->
	Val = eval(Exp, Env),
	case env:update(Name, Val, Env) of
		ok -> Val;
		_ -> io:format("Failed to set variable ~p~n", [Name]), error
	end;

eval({add, L, R}, Env) -> eval(L, Env) + eval(R, Env);
eval({sub, L, R}, Env) -> eval(L, Env) - eval(R, Env);
eval({mul, L, R}, Env) -> eval(L, Env) * eval(R, Env);
eval({'div', L, R}, Env) -> eval(L, Env) div eval(R, Env);

eval({lt, L, R}, Env) -> bool(eval(L, Env) < eval(R, Env));
eval({gt, L, R}, Env) -> bool(eval(L, Env) > eval(R, Env));
eval({lte, L, R}, Env) -> bool(eval(L, Env) =< eval(R, Env));
eval({gte, L, R}, Env) -> bool(eval(L, Env) >= eval(R, Env));
eval({eq, L, R}, Env) -> bool(eval(L, Env) == eval(R, Env));
eval({neq, L, R}, Env) -> bool(eval(L, Env) /= eval(R, Env));

eval({deref, Id}, Env) -> 
	case env:get(Id, Env) of
		{ok, Value} -> Value;
		error -> io:format("Referenced undeclared variable ~p~n", [Id]), error
	end;
eval(X, _) when is_number(X) -> X.

