" Vim syntax file
" Language:     FMA

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword fmaCommand theorem fun prop eval proof end claim
syn keyword fmaFormula forall exists
syn keyword fmaProof let suppose indeed by induction contradiction show
syn keyword fmaProof then also have assume cases case take consider st
syn region fmaComment start="--" end="$" keepend
syn region fmaComment start="{-" end="-}" keepend

hi def link fmaCommand  Structure
hi def link fmaFormula  Constant
hi def link fmaProof    Keyword
hi def link fmaComment  Comment

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "fma"

