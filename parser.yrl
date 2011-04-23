
Nonterminals lines stm exp e arg_list arg_def_list funcall let_list.
Terminals '+' '-' '*' '/' '(' ')' '=' ',' ';' '<' '>' '>=' '<=' '!=' '=='
	eol int id 'let' 'end' def return 'if' then else for do while.

Rootsymbol lines.

Right 50 arg_list.
Left 100 ','.
Left 400 '<' '>' '!=' '==' '<=' '>='.
Left 500 '+' '-'.
Left 600 '*' '/'.

lines -> stm : ['$1'].
lines -> stm e : ['$1'].
lines -> stm e lines : ['$1' | '$3'].
lines -> e lines : '$2'.
lines -> e : [].

e -> eol.
e -> ';'.

let_list -> id '=' exp : [{v('$1'), '$3'}].
let_list -> id '=' exp ',' let_list : [{v('$1'), '$3'} | '$5'].
let_list -> id '=' exp ',' eol let_list : [{v('$1'), '$3'} | '$6'].

stm -> 'let' let_list : {'let', '$2'}.

stm -> 'def' id '(' arg_def_list ')' lines 'end' : {fundef, v('$2'), '$4', '$6'}.

stm -> exp : '$1'.

stm -> 'if' exp then lines end : {'if', '$2', '$4', none}.
stm -> 'if' exp then lines else lines end : {'if', '$2', '$4', '$6'}.

stm -> for id '=' exp ',' exp do lines end : {for, v('$2'), '$4', '$6', '$8'}.

stm -> while exp do lines end : {while, '$2', '$4'}.

stm-> return : {return}.
stm-> return exp : {return, '$2'}.

exp -> exp '+' exp : {add, '$1', '$3'}.
exp -> exp '-' exp : {sub, '$1', '$3'}.
exp -> exp '*' exp : {mul, '$1', '$3'}.
exp -> exp '/' exp : {'div', '$1', '$3'}.

exp -> exp '<' exp : {lt, '$1', '$3'}.
exp -> exp '>' exp : {gt, '$1', '$3'}.
exp -> exp '<=' exp : {lte, '$1', '$3'}.
exp -> exp '>=' exp : {gte, '$1', '$3'}.
exp -> exp '==' exp : {eq, '$1', '$3'}.
exp -> exp '!=' exp : {neq, '$1', '$3'}.

exp -> '(' exp ')' : '$2'.
exp -> int : v('$1').
exp -> id : {deref, v('$1')}.
exp -> funcall : '$1'.

exp -> id '=' exp : {assign, v('$1'), '$3'}.

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

