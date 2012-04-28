" Honu syntax file
" Language: Honu
" Filenames: *.honu
" Maintainers: Jon Rafkind <jon@rafkind.com>
" URL: http://racket-lang.org
" Last Change: 2012 April 24 - Initial Version

if version < 600
  syntax clear
elseif exists("b:current_syntax") && b:current_syntax == "fortress"
  finish
endif

syn region honuComment start="/\*" end="\*/" contains=honuComment
syn match honuLineComment "//.*"

syn region honuString start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match honuNumber "\<-\=\d\(_\|\d\)*"

syn keyword honuKeyword pattern macro var function
syn keyword honuConditional if
syn match honuOperator "="
syn keyword honuBoolean true false
syn match honuSpecial "\.\.\."
syn match honuSpecial "\$"
syn match honuSpecial "#lang"

if version >= 508 || !exists("did_honu_syntax_inits")
  if version < 508
    let did_honu_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink honuComment           Comment
  HiLink honuLineComment       Comment
  HiLink honuKeyword           Keyword
  HiLink honuExternal          Include
  HiLink honuType              Type
  HiLink honuOperator          Operator
  HiLink honuEnd               Statement

  HiLink honuBoolean           Boolean
  HiLink honuConditional       Conditional
  
  HiLink honuString            String
  HiLink honuChar              String
  HiLink honuNumber            Number
  HiLink honuSpecial           Special
  
  HiLink honuThenErr           Error
  
  delcommand HiLink
endif

let b:current_syntax = "honu"
