filetype plugin indent on
syntax enable

packadd fzf
packadd fzf-vim
packadd tabular

" functions
function! s:gitBlame(bufnr, filename, ...)
  execute 'leftabove 40 vnew'
  execute 'autocmd BufWipeout <buffer> call setbufvar(' . a:bufnr .', "&cursorbind", 0)'
  execute 'read!git blame --date short --minimal ' . shellescape(a:filename)
  set buftype=nofile bufhidden=wipe nowrap noswapfile nonumber norelativenumber cursorbind nowrap foldcolumn=0 nofoldenable winfixwidth filetype=git
  0delete _
  wincmd p
  set cursorbind
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

function! s:searchWithVimgrep(query)
  exe "vimgrep /".a:query."/g **/*"
endfunction

function! s:searchTags(query)
  exe "tselect /".a:query
endfunction

function! s:buildQuickfixList(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

function! s:loadPluginsConfig()
  if !empty(expand(glob("~/.vim/status.vim")))
  endif
endfunction

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

" settings
set backspace=indent,eol,start
set clipboard=unnamedplus
set cmdheight=2
set colorcolumn=80,160
set cursorline
set dir=/tmp
set encoding=utf-8
set expandtab
set foldcolumn=1
set foldmethod=manual
set formatprg=par
set grepformat=%f:%l:%c:%m
set grepprg=rg\ --vimgrep\ $*
set hidden
set hlsearch
set incsearch
set laststatus=2
set list
set listchars=tab:»\ ,nbsp:␣,trail:·,extends:›,precedes:‹
set mouse=a
set nobackup
set nocompatible
set noshowmode
set nospell
set nowritebackup
set number
set path+=**
set relativenumber
set shiftwidth=2
set shortmess+=c
set showcmd
set signcolumn=yes
set softtabstop=2
set splitbelow
set splitright
set swapfile
set tabstop=2
set tags+=.git/tags;
set termencoding=utf-8
set timeoutlen=1000 ttimeoutlen=0
set ttyfast
set updatetime=300
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.rar,*.zip,*.tar.*,*.bmp,*.jpeg,*.avi,*.mov,*.mp7,*.ogg,*.flac
set wildmenu
set wrapmargin=0

" status
set statusline=
set statusline+=%#StatusLineMode#
set statusline+=%{StatusLineMode()}
set statusline+=%#StatusLineInfo#
set statusline+=\ %n
set statusline+=%#StatusLineNormal#
set statusline+=\ %f:%l:%c
set statusline+=%#StatusLineInfo#%=
set statusline+=\ %m
set statusline+=\ %y
set statusline+=\ \[%{&fileencoding?&fileencoding:&encoding}\]
set statusline+=\ \[%{&fileformat}\]
set statusline+=\ \[%p%%\ %L\]

" options
let $FZF_DEFAULT_COMMAND = 'git ls-files'
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
let g:fzf_action = { 'ctrl-q': function('s:buildQuickfixList'), 'ctrl-t': 'tab split', 'ctrl-o': 'split', 'ctrl-v': 'vsplit' }
let g:fzf_tags_command = 'git ctags'
let g:mapleader = "\\"
let g:maplocalleader = ","
let g:netrw_altv = 1
let g:netrw_liststyle = 3
let g:netrw_preview = 1

" mapping
nnoremap <leader>1  :set number!<CR>
nnoremap <leader>2  :set relativenumber!<CR>
nnoremap <leader>3  :set hlsearch!<CR>
nnoremap <leader>v  :vsplit<CR>
nnoremap <leader>o  :split<CR>
nnoremap <leader>\  :Vexplore<CR>
nnoremap <leader>/  :VexploreFind<CR>
nnoremap <leader>b  :Buffers<CR>
nnoremap <leader>B  :bufdo bd<CR>
nnoremap <leader>c  :terminal ++close ++rows=8<CR>
nnoremap <leader>d  :diffthis<CR>
nnoremap <leader>s  :set cursorbind!<CR>
nnoremap <leader>f  :Rg<CR>
nnoremap <leader>F  :FZF<CR>
nnoremap <leader>gc :Commits<CR>
nnoremap <leader>gf :GFiles<CR>
nnoremap <leader>gs :GFiles?<CR>
nnoremap <leader>h  :History<CR>
nnoremap <leader>m  :Marks<CR>
nnoremap <leader>t  :Tags<CR>
nnoremap <leader>T  :BTags<CR>

nmap <silent>       [b :bprevious<CR>
nmap <silent>       ]b :bnext<CR>
nmap <localleader>b :Blame<CR>

vnoremap <silent> * :call setreg("/", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent> # :call setreg("?", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent> f :<C-u>call <SID>searchWithRg(<SID>getSelectedText())<CR>
vnoremap <silent> F :<C-u>call <SID>searchWithVimgrep(<SID>getSelectedText())<CR>
vnoremap <silent> t :<C-u>call fzf#vim#tags(<SID>getSelectedText())<CR>
vnoremap <silent> T :<C-u>call <SID>searchTags(<SID>getSelectedText())<CR>

" commands
command! VexploreFind let @/=expand("%:t") | execute 'Vexplore' expand("%:h") | normal n
command! MakeTags     !git ctags
command! Open         !open %
command! -count Blame call <SID>gitBlame(bufnr('%'), expand('%:p'), <f-args>)

" colors
hi ColorColumn      ctermbg=18
hi DiffAdd          ctermbg=2 ctermfg=0 cterm=BOLD
hi DiffChange       ctermbg=3 ctermfg=0 cterm=BOLD
hi DiffDelete       ctermbg=1 ctermfg=0 cterm=BOLD
hi DiffText         ctermbg=2 ctermfg=0 cterm=BOLD
hi Directory        ctermfg=4
hi FoldColumn       ctermbg=0 ctermfg=7
hi Folded           ctermbg=6 ctermfg=0
hi Pmenu            ctermbg=8
hi Search           ctermbg=2 ctermfg=0
hi SignColumn       ctermbg=0
hi StatusLineMode   ctermbg=9 ctermfg=0 cterm=BOLD
hi StatusLineNormal ctermbg=0 ctermfg=7
hi StatusLineinfo   ctermbg=0 ctermfg=8 cterm=BOLD
hi TabLine          ctermbg=0 ctermfg=7 cterm=NONE
hi TabLineFill      ctermbg=0 ctermfg=0
hi TabLineSel       ctermbg=0 ctermfg=9 cterm=NONE
hi VertSplit        ctermbg=8 ctermfg=8
hi Visual           ctermbg=7 ctermfg=0
hi netrwComment     ctermfg=8
hi netrwTreeBar     ctermfg=8

" autocommands
autocmd! BufWritePre * :%s/\s\+$//e
autocmd! BufWritePost * :silent! MakeTags
autocmd! FileType git noremap yy 0viwy
autocmd! FileType make setlocal noexpandtab
autocmd! FileType haskell setlocal makeprg=ghcid
autocmd! FileType haskell packadd haskell-vim
autocmd! FileType fzf set laststatus=0 noshowmode noruler | autocmd BufLeave <buffer> set laststatus=2 showmode ruler
