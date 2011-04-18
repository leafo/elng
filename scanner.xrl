Definitions.

D = [0-9]
SYM = \+|-|\*|/|\(|\)|=|,|;
ATOM = [a-z][0-9a-zA-Z_]*

Rules.

{D}+		: {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{SYM}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{ATOM}		: Atom = list_to_atom(TokenChars),
				{token, 
					case r(Atom) of
						true -> {Atom, TokenLine};
						_ -> {id, TokenLine, TokenChars}
					end}.
\n			: {token, {eol, TokenLine}}.
\s|\t|		: skip_token.

Erlang code.

r('let') -> true;
r('def') -> true;
r('end') -> true;
r('return') -> true;
r(_) -> false.
