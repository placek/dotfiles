function! Ask(args)
  let l:command="cexpr system('find . -not -wholename \".git*\" -not -iname \"tags\" -exec grep -Hn \"".a:args."\" {} \\;')"
  exec l:command
  exec "copen"
endfunction

function! Find(args)
  let l:command="cexpr system('find . -iname \"".a:args."\" -printf \"%p:0: \\n\"')"
  exec l:command
  exec "copen"
endfunction

command! -nargs=* Find :call Find(<q-args>)
command! -nargs=* Ask :call Ask(<q-args>)
