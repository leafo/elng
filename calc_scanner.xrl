Definitions.

D = [0-9]
SYM = \+|-|\*|/|\(|\)

Rules.

{D}+	: {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{SYM}	: {token, {list_to_atom(TokenChars), TokenLine}}.
\s		: skip_token.


Erlang code.
