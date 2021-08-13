set laststatus=2
set statusline=
set statusline+=%#StatusLineMode#
set statusline+=%{StatusLineMode()}
set statusline+=%#StatusLineGit#
set statusline+=\ %{b:gitbranch}
set statusline+=%#StatusLineInfo#
set statusline+=%n
set statusline+=%#StatusLineNormal#
set statusline+=\ %f:%l:%c
set statusline+=%#StatusLineInfo#%=
set statusline+=\ %m
set statusline+=\ %y
set statusline+=\ \[%{&fileencoding?&fileencoding:&encoding}\]
set statusline+=\ \[%{&fileformat}\]
set statusline+=\ \[%p%%\ %L\]

hi StatusLineMode   ctermbg=9 ctermfg=0 cterm=BOLD
hi StatusLineNormal ctermbg=0 ctermfg=7
hi StatusLineGit    ctermbg=0 ctermfg=3
hi StatusLineinfo   ctermbg=0 ctermfg=8 cterm=BOLD
hi TabLineFill      ctermbg=0 ctermfg=0
hi TabLine          ctermbg=0 ctermfg=7 cterm=NONE
hi TabLineSel       ctermbg=0 ctermfg=9 cterm=NONE

function! StatusLineMode()
  let l:mode=mode()
  if l:mode==#"n"
    return "  NORMAL "
  elseif l:mode==?"v"
    return "  VISUAL "
  elseif l:mode==#"i"
    return "  INSERT "
  elseif l:mode==#"R"
    return "  REPLACE "
  endif
endfunction

function! StatusLineGitBranch()
  let b:gitbranch=""
  if &modifiable
    lcd %:p:h
    let l:gitrevparse=system("git rev-parse --abbrev-ref HEAD")
    lcd -
    if l:gitrevparse!~"fatal: "
      let b:gitbranch=substitute(l:gitrevparse, '\n', '', 'g')." "
    endif
  endif
endfunction

augroup GetGitBranch
  autocmd!
  autocmd VimEnter,WinEnter,BufEnter * call StatusLineGitBranch()
augroup END
