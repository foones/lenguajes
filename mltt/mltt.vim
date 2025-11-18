" Vim syntax file
" Language:     MLTT

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword mlttCommand axiom def check rewrite
syn keyword mlttKeyword Type
syn region mlttComment start="--" end="$" keepend
syn region mlttComment start="{-" end="-}" keepend

hi def link mlttCommand  Structure
hi def link mlttKeyword  Keyword
hi def link mlttComment  Comment

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "fol"

