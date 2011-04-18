-module(env).
-compile(export_all).

new(Parent) ->
	spawn(?MODULE, loop, [dict:new(), Parent]).

new() -> new(none).

loop(Dict, Parent) ->
	receive 
		{set, Name, Value, Pid} ->
			Pid ! {set, Name},
			loop(dict:store(Name, Value, Dict), Parent);
		{update, Name, Value, Pid} ->
			case dict:is_key(Name, Dict) of 
				true ->
					Pid ! {update, Name},
					loop(dict:store(Name, Value, Dict), Parent);
				false ->
					case Parent of
						none -> Pid ! {error, update, Name};
						_ -> Parent ! {update, Name, Value, Pid}
					end,
					loop(Dict, Parent)
			end;
		{get, Name, Pid} ->
			case dict:find(Name, Dict) of
				{ok, Value} -> Pid ! {get, Name, Value};
				error ->
					case Parent of
						none -> Pid ! {error, get, Name};
						_ -> Parent ! {get, Name, Pid}
					end
			end,
			loop(Dict, Parent)
	end.

set(Name, Value, Env) ->
	Env ! {set, Name, Value, self()},
	receive {set, Name} -> ok end.

update(Name, Value, Env) ->
	Env ! {update, Name, Value, self()},
	receive
		{update, Name} -> ok;
		{error, update, Name} -> error
	end.

get(Name, Env) ->
	Env ! {get, Name, self()},
	receive
		{get, Name, Value} -> {ok, Value};
		{error, get, Name} -> error
	end.

test() ->
	D1 = new(),
	D2 = new(D1),

	set(hello, 24305, D1),
	set(world, 78923, D2),

	{ok, 24305} = get(hello, D2),
	{ok, 24305} = get(hello, D2),

	error = get(world, D1),
	{ok, 24305} = get(hello, D1),
	{ok, 78923} = get(world, D2),

	ok = update(hello, 12345, D2),

	{ok, 12345} = get(hello, D2),
	{ok, 12345} = get(hello, D1),

	error = update(world, 45321, D1),
	ok = update(world, 45321, D2),

	{ok, 45321} = get(world, D2),

	ok.


