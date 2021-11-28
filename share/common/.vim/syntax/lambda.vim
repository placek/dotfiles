if exists("b:current_syntax")
  finish
endif

syntax keyword lambdaKeyword      let in
syntax match   lambdaComment      "--.*"
syntax match   lambdaAbstraction  "\\"
syntax match   lambdaDot          "\."
syntax match   lambdaOpenBracket  "("
syntax match   lambdaCloseBracket ")"
syntax match   lambdaPhraseEnd    ";"
syntax match   lambdaVariable     /\\\zs[a-zA-Z0-9_]\+/

highlight link lambdaKeyword     Keyword
highlight link lambdaComment     Comment
highlight link lambdaAbstraction Operator
highlight link lambdaDot         Operator
highlight link lambdaVariable    String

let b:current_syntax = "lambda"
