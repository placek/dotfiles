packadd vim-gitgutter

" functions
function! s:GitBlame(bufnr, filename, ...)
  execute 'leftabove 40 vnew'
  execute 'autocmd BufWipeout <buffer> call setbufvar(' . a:bufnr .', "&cursorbind", 0)'
  execute 'read!git blame --date short --minimal ' . shellescape(a:filename)
  set buftype=nofile bufhidden=wipe nowrap noswapfile nonumber norelativenumber cursorbind nowrap foldcolumn=0 nofoldenable winfixwidth filetype=gitblame previewwindow
  0delete _
  wincmd p
  set cursorbind
endfunction

" options
let g:gitgutter_map_keys = 0

" mappings
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
nmap ghs <Plug>(GitGutterStageHunk)
nmap ghu <Plug>(GitGutterUndoHunk)
nmap ghp <Plug>(GitGutterPreviewHunk)
omap ih <Plug>(GitGutterTextObjectInnerPending)
omap ah <Plug>(GitGutterTextObjectOuterPending)
xmap ih <Plug>(GitGutterTextObjectInnerVisual)
xmap ah <Plug>(GitGutterTextObjectOuterVisual)

nmap <leader>gb :GitBlame<CR>

" commands
command! -count GitBlame call <SID>GitBlame(bufnr('%'), expand('%:p'), <f-args>)
