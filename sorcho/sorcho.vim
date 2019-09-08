" Vim syntax file
" Language:	Sorcho Kang
" Maintainer:	Pablo Barenbaum <pablob@starlinux.net>
" Last Change:	2004 Oct 17

" VI-lmente robado del archivo de sintaxis de Haskell.

" Remove any old syntax stuff hanging around
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

" (Qualified) identifiers (no default highlighting)
syn match ConId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[A-Z][a-zA-Z0-9_']*\>"
syn match VarId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_']*\>"

" Infix operators--most punctuation characters and any (qualified) identifier
" enclosed in `backquotes`. An operator starting with : is a constructor,
" others are variables (e.g. functions).
syn match srVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match srConSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn match srVarSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
syn match srConSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"

" Reserved symbols--cannot be overloaded.
syn match srDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}"

" Strings and constants
syn match   srSpecialChar	contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   srSpecialChar	contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   srSpecialCharError	contained "\\&\|'''\+"
syn region  srString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=srSpecialChar
syn match   srCharacter		"[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=srSpecialChar,srSpecialCharError
syn match   srCharacter		"^'\([^\\]\|\\[^']\+\|\\'\)'" contains=srSpecialChar,srSpecialCharError
syn match   srNumber		"\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match   srFloat		"\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn region  srFungeoid		matchgroup=srDelimiter start=+{+ end=+^[ \t]*}$+

" Keyword definitions. These must be patters instead of keywords
" because otherwise they would match as keywords at the start of a
" "literate" comment (see lhs.vim).
syn match srModule		"\<module\>"
syn match srInfix		"\<\(infix\|infixl\|infixr\)\>"
syn match srStructure		"\<\(che\|please\)\>"
syn match srStructure		"\<\(pubic\|cubic\|rubik\|magic\|hladik\|magic\|plastic\|void\|lizard\|estafrio\|warp\)\>"
syn match srConditional		"\<\(si\|o\|sino\)\>"

" Comments
syn match   srLineComment      "::.*"

" Literate comments--any line not starting with '>' is a comment.
if exists("b:hs_literate_comments")
  syn region  srLiterateComment   start="^" end="^>"
endif

if !exists("hs_minlines")
  let sr_minlines = 50
endif
exec "syn sync lines=" . sr_minlines

if version >= 508 || !exists("did_hs_syntax_inits")
  if version < 508
    let did_hs_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  hi link srModule			  srStructure
  hi link srImport			  Include
  hi link srImportMod			  srImport
  hi link srInfix			  PreProc
  hi link srStructure			  Structure
  hi link srStatement			  Statement
  hi link srConditional			  Conditional
  hi link srSpecialChar			  SpecialChar
  hi link srTypedef			  Typedef
  hi link srVarSym			  srOperator
  hi link srConSym			  srOperator
  hi link srOperator			  Operator
  ""hi link srDelimiter			  Delimiter
  hi link srSpecialCharError		  Error
  hi link srString			  String
  hi link srFungeoid			  String
  hi link srCharacter			  Character
  hi link srNumber			  Number
  hi link srFloat			  Float
  hi link srConditional			  Conditional
  hi link srLiterateComment		  srComment
  hi link srBlockComment		  srComment
  hi link srLineComment			  srComment
  hi link srComment			  Comment
  hi link srPragma			  SpecialComment
  hi link srBoolean			  Boolean
  hi link srType			  Type
  hi link srMaybe			  srEnumConst
  hi link srOrdering			  srEnumConst
  hi link srEnumConst			  Constant
  hi link srDebug			  Debug

  delcommand HiLink
endif

let b:current_syntax = "sorcho"

" Options for vi: ts=8 sw=2 sts=2 nowrap noexpandtab ft=vim
