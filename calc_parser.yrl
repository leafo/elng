
Nonterminals exp.
Terminals '+' '-' '*' '/' '(' ')' int.

Rootsymbol exp.

Left 100 '+' '-'.
Left 200 '*' '/'.

exp -> exp '+' exp : {add, '$1', '$3'}.
exp -> exp '-' exp : {sub, '$1', '$3'}.
exp -> exp '*' exp : {mul, '$1', '$3'}.
exp -> exp '/' exp : {'div', '$1', '$3'}.
exp -> '(' exp ')' : '$2'.
exp -> int : v('$1').

Erlang code.

v({_, _, Value}) -> Value.

