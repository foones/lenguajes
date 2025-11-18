" Vim syntax file
" Language:     Fol

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword folStructure axiom data theorem prop end claim cases case induction
syn keyword folCommand by have then suppose let take consider st
syn keyword folFinalCommand contradiction hence conclude thus
syn keyword folFormula forall exists true false
syn keyword folWarning admit
syn match   folWarning "\.\.\."
syn region folComment start="--" end="$" keepend

hi def link folStructure     Structure
hi def link folCommand       Keyword
hi def link folFinalCommand  Preproc
hi def link folFormula       Reserved
hi def link folComment       Comment
hi def link folWarning       Error

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "fol"

