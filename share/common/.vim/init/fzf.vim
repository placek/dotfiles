packadd fzf
packadd fzf-vim

" functions
function! s:buildQuickfixList(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

function! s:getSelectedText()
  norm gv"sy
  let l:ret = getreg('s')
  exe "norm \<Esc>"
  return l:ret
endfunction

function! s:searchWithRg(query)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), 0)
endfunction

" options
let $FZF_DEFAULT_COMMAND = 'git ls-files'
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
let g:fzf_action = { 'ctrl-q': function('s:buildQuickfixList'), 'ctrl-t': 'tab split', 'ctrl-o': 'split', 'ctrl-v': 'vsplit' }
let g:fzf_tags_command = 'git ctags'

" mappings
nnoremap <leader>F  :FZF<CR>
nnoremap <leader>T  :BTags<CR>
nnoremap <leader>b  :Buffers<CR>
nnoremap <leader>f  :Rg<CR>
nnoremap <leader>gc :Commits<CR>
nnoremap <leader>gf :GFiles<CR>
nnoremap <leader>gs :GFiles?<CR>
nnoremap <leader>h  :History<CR>
nnoremap <leader>m  :Marks<CR>
nnoremap <leader>t  :Tags<CR>
vnoremap <silent> f :<C-u>call <SID>searchWithRg(<SID>getSelectedText())<CR>
vnoremap <silent> t :<C-u>call fzf#vim#tags(<SID>getSelectedText())<CR>

" autocommands
autocmd! FileType fzf set laststatus=0 noshowmode noruler | autocmd BufLeave <buffer> set laststatus=2 showmode ruler
