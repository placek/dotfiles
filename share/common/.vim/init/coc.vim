packadd coc.nvim
packadd coc-fzf

" functions
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" mappings
nnoremap <silent>K        :call <SID>show_documentation()<CR>
nmap <silent> [c          <Plug>(coc-git-prevconflict)
nmap <silent> ]c          <Plug>(coc-git-nextconflict)
nmap <silent> [g          <Plug>(coc-diagnostic-prev)
nmap <silent> ]g          <Plug>(coc-diagnostic-next)
nmap <silent> [h          <Plug>(coc-git-prevchunk)
nmap <silent> ]h          <Plug>(coc-git-nextchunk)
xmap <silent> ic          <Plug>(coc-classobj-i)
omap <silent> ic          <Plug>(coc-classobj-i)
xmap <silent> ac          <Plug>(coc-classobj-a)
omap <silent> ac          <Plug>(coc-classobj-a)
xmap <silent> if          <Plug>(coc-funcobj-i)
omap <silent> if          <Plug>(coc-funcobj-i)
xmap <silent> af          <Plug>(coc-funcobj-a)
omap <silent> af          <Plug>(coc-funcobj-a)
omap <silent> ih          <Plug>(coc-git-chunk-inner)
xmap <silent> ih          <Plug>(coc-git-chunk-inner)
omap <silent> ah          <Plug>(coc-git-chunk-outer)
xmap <silent> ah          <Plug>(coc-git-chunk-outer)
nmap <localleader>a       <Plug>(coc-codeaction-cursor)
nmap <localleader>c       <Plug>(coc-git-commit)
nmap <localleader>d       <Plug>(coc-definition)
nmap <localleader>f       <Plug>(coc-format)
vmap <localleader>f       <Plug>(coc-format-selected)
nmap <localleader>g       <Plug>(coc-diagnostic-info)
nmap <localleader>h       <Plug>(coc-git-chunkinfo)
nmap <localleader>i       <Plug>(coc-implementation)
nmap <localleader>r       <Plug>(coc-rename)
nmap <localleader>q       <Plug>(coc-refactor)
nmap <localleader>R       <Plug>(coc-references)
nmap <localleader>t       <Plug>(coc-type-definition)
nmap <localleader><space> :<C-u>CocFzfList<CR>
nmap <leader>\            :CocCommand explorer --toggle --sources=file+<CR>

" autocmd
autocmd CursorHold * silent call CocActionAsync('highlight')

" commands
command! -nargs=0 Format :call CocAction('format')
command! -nargs=? Fold   :call CocAction('fold', <f-args>)
command! -nargs=0 OR     :call CocActionAsync('runCommand', 'editor.action.organizeImport')

" options
let g:coc_global_extensions = ['coc-tag', 'coc-git', 'coc-explorer']

" colors
hi CocGitAddedSign         ctermbg=0  ctermfg=2
hi CocExplorerGitAdded     ctermbg=0  ctermfg=2
hi CocGitChangeRemovedSign ctermbg=0  ctermfg=3
hi CocGitChangedSign       ctermbg=0  ctermfg=16
hi CocExplorerGitModified  ctermbg=0  ctermfg=16
hi CocGitRemovedSign       ctermbg=0  ctermfg=1
hi CocExplorerGitDeleted   ctermbg=0  ctermfg=1
hi CocGitTopRemovedSign    ctermbg=0  ctermfg=1
hi CocHighlightText        ctermbg=18 ctermfg=2

" status
set statusline=
set statusline+=%#StatusLineMode#
set statusline+=%{StatusLineMode()}
set statusline+=%#StatusLineInfo#
set statusline+=\ %n
set statusline+=%#StatusLine#
set statusline+=\ %f:%l:%c
set statusline+=\ %{coc#status()}
set statusline+=%#StatusLineInfo#%=
set statusline+=\ %m
set statusline+=\ %y
set statusline+=\ \[%{&fileencoding?&fileencoding:&encoding}\]
set statusline+=\ \[%{&fileformat}\]
set statusline+=\ \[%p%%\ %L\]
