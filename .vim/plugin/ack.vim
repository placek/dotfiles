" NOTE: You must, of course, install the ack script
"       in your path.
" On Ubuntu:
"   sudo apt-get install ack-grep
"   ln -s /usr/bin/ack-grep /usr/bin/ack
" With MacPorts:
"   sudo port install p5-app-ack

let g:ackprg="ask\\ -H\\ --nocolor\\ --nogroup"

function! Ask(args)
    let grepprg_bak=&grepprg
    exec "set grepprg=" . g:ackprg
    execute "silent! grep " . a:args
    botright copen
    let &grepprg=grepprg_bak
    exec "redraw!"
endfunction

function! AskAdd(args)
    let grepprg_bak=&grepprg
    exec "set grepprg=" . g:ackprg
    execute "silent! grepadd " . a:args
    botright copen
    let &grepprg=grepprg_bak
    exec "redraw!"
endfunction

function! LAsk(args)
    let grepprg_bak=&grepprg
    exec "set grepprg=" . g:ackprg
    execute "silent! lgrep " . a:args
    botright lopen
    let &grepprg=grepprg_bak
    exec "redraw!"
endfunction

function! LAskAdd(args)
    let grepprg_bak=&grepprg
    exec "set grepprg=" . g:ackprg
    execute "silent! lgrepadd " . a:args
    botright lopen
    let &grepprg=grepprg_bak
    exec "redraw!"
endfunction

function! Find(...)
  let path="."
  if a:0==2
    let path=a:2
  endif
  let l:list=system("find ".path. " -iname '".a:1."'")
  let l:num=strlen(substitute(l:list, "[^\n]", "", "g"))
  if l:num < 1
    echo "'".a:1."' not found"
    return
  endif
  if l:num == 1
    exe "open " . substitute(l:list, "\n", "", "g")
    else
    let tmpfile = tempname()
    exe "redir! > " . tmpfile
    silent echon l:list
    redir END
    let old_efm = &efm
    set efm=%f
    if exists(":cgetfile")
      execute "silent! cgetfile " . tmpfile
    else
      execute "silent! cfile " . tmpfile
    endif
    let &efm = old_efm
    botright
    copen
    call delete(tmpfile)
  endif
endfunction

command! -nargs=* Find :call Find(<f-args>)
command! -nargs=* -complete=file Ask call Ask(<q-args>)
command! -nargs=* -complete=file AskAdd call AskAdd(<q-args>)
command! -nargs=* -complete=file LAsk call LAsk(<q-args>)
command! -nargs=* -complete=file LAskAdd call LAskAdd(<q-args>)
