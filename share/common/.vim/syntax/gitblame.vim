" Vim syntax file
" Language: git blame

if exists("b:current_syntax")
  finish
end

let b:current_syntax = "gitblame"

" syntax
syn match gitHash "^\<[0-9a-f]\+\>"
syn match gitDate "\<[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}\>"
syn match gitEmail "(\<.*\(\s\d\{4\}\)\@="
