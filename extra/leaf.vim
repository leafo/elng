" Vim syntax file leaf files
"
if exists("b:current_syntax")
	finish
endif

syn keyword leafConstructs def if then else end case dispatch for in do while
syn keyword leafBuiltins print return
syn keyword leafActions let 

syn region leafString start=+"+ end=+"+

syn match leafBraces "[\[\]]"
syn match leafOperators "[-+*/%=<>]"
syn match leafOperators "[<>!=]="

syn match leafInteger "\d\+"

syn match leafComment "#.*$" contains=@Spell


let b:current_syntax = "leaf"

hi def link leafConstructs Type
hi def link leafBuiltins Conditional
hi def link leafActions Identifier
hi def link leafString String
hi def link leafBraces Function
hi def link leafOperators Number
hi def link leafComment Comment

