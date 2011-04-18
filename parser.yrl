
Nonterminals lines stm exp e arg_list arg_def_list funcall.
Terminals '+' '-' '*' '/' '(' ')' '=' ',' ';'
	eol int id 'let' 'end' def return.

Rootsymbol lines.

Right 50 arg_list.
Left 75 ','.
Left 100 '+' '-'.
Left 200 '*' '/'.

lines -> stm : ['$1'].
lines -> stm e : ['$1'].
lines -> stm e lines : ['$1' | '$3'].
lines -> e lines : '$2'.
lines -> e : [].

e -> eol.
e -> ';'.

stm -> 'let' id '=' exp : {'let', v('$2'), '$4'}.
stm -> 'def' id '(' arg_def_list ')' lines 'end' : {fundef, v('$2'), '$4', '$6'}.

stm -> exp : '$1'.

stm-> return : {return}.
stm-> return exp : {return, '$2'}.

exp -> exp '+' exp : {add, '$1', '$3'}.
exp -> exp '-' exp : {sub, '$1', '$3'}.
exp -> exp '*' exp : {mul, '$1', '$3'}.
exp -> exp '/' exp : {'div', '$1', '$3'}.
exp -> '(' exp ')' : '$2'.
exp -> int : v('$1').
exp -> id : {deref, v('$1')}.
exp -> funcall : '$1'.

funcall -> id arg_list : {funcall, v('$1'), '$2'}.
funcall -> id '(' arg_list ')' : {funcall, v('$1'), '$3'}.
funcall -> id '(' ')' : {funcall, v('$1'), []}.

arg_list -> exp ',' arg_list : ['$1' | '$3'].
arg_list -> exp : ['$1'].

arg_def_list -> id ',' arg_def_list : [v('$1') | '$3'].
arg_def_list -> id : [v('$1')].
arg_def_list -> '$empty' : [].

Erlang code.

v({_, _, Value}) -> Value.

