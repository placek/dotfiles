call plug#begin('~/.vim/bundle')
  Plug 'scrooloose/nerdtree'
  Plug 'xuyuanp/nerdtree-git-plugin'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
  Plug 'terryma/vim-multiple-cursors'
  Plug 'airblade/vim-gitgutter'
  Plug 'tpope/vim-surround'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'w0rp/ale'
  Plug 'mattn/emmet-vim'
  Plug 'godlygeek/tabular'
call plug#end()

filetype plugin indent on
syntax enable

packadd! matchit

" settings
set backspace=indent,eol,start
set backup
set backupdir=/tmp
set clipboard=unnamed
set completeopt=longest,menuone
set cursorline
set dir=/tmp
set expandtab
set fo=tcrqn
set foldmethod=indent
set hlsearch
set incsearch
set laststatus=2
set listchars=tab:→\ ,space:·,eol:¬,nbsp:◦
set mouse=a
set nocompatible
set nospell
set ofu=syntaxcomplete#Complete
set rtp+=/usr/local/opt/fzf
set shell=/usr/local/bin/bash
set shiftwidth=2
set showcmd
set softtabstop=2
set showtabline=2
set splitbelow
set splitright
set noshowmode
set swapfile
set tabstop=2
set tags+=.git/tags;
set textwidth=0
set term=xterm-256color
set timeoutlen=2000 ttimeoutlen=0
set undodir=/tmp
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.rar,*.zip,*.tar.*,*.bmp,*.jpeg,*.avi,*.mov,*.mp7,*.ogg,*.flac
set wrapmargin=0

map  <ESC>[H <Home>
map  <ESC>[F <End>
imap <ESC>[H <Home>
imap <ESC>[F <End>

" mapping
nnoremap <Leader>v :vsplit<CR>
nnoremap <Leader>s :split<CR>
nnoremap <Leader>\ :NERDTreeToggle<CR>
nnoremap <Leader>1 :set number!<CR>
nnoremap <Leader>2 :call FoldColumnToggle()<CR>
nnoremap <Leader>3 :GitGutterToggle<CR>
nnoremap <Leader>4 :set hlsearch!<CR>
nnoremap <Leader>5 :set list!<CR>
nnoremap <Leader>t :FZF<CR>
nnoremap <Leader>f :Ag<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>c :Commits<CR>
nnoremap <Leader>T :Tags<CR>
nnoremap <Leader>S :GFiles?<CR>
nnoremap <Leader>G :GFiles<CR>
nnoremap <Leader>q *``cgn
nnoremap <Leader>r :NERDTreeFind<CR>
nnoremap <Leader>h <C-W><C-H>
nnoremap <Leader>j <C-W><C-J>
nnoremap <Leader>k <C-W><C-K>
nnoremap <Leader>l <C-W><C-L>
nnoremap <silent>* *``
nmap ]h <Plug>GitGutterNextHunk
nmap [h <Plug>GitGutterPrevHunk
vnoremap // y/<C-R>"<CR>

if exists(":Tabularize")
  nnoremap <Leader>a{ :Tabularize /{<CR>
  vnoremap <Leader>a{ :Tabularize /{<CR>
endif

nnoremap <silent> <2-LeftMouse> *``

" options
let g:fzf_tags_command = 'git ctags'
let g:airline_theme = 'solarized'

" autocommands
augroup normalize
  autocmd BufWritePre * :%s/\s\+$//e
  autocmd BufRead * normal zR
augroup END

augroup ruby_file
  autocmd Filetype ruby setlocal makeprg=rubocop\ %
augroup END

augroup haskell_file
  autocmd Filetype haskell setlocal makeprg=hlint\ %
augroup END

autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
autocmd FileType make setlocal noexpandtab

" colors
hi FoldColumn ctermbg=7 ctermfg=0
hi LineNr ctermbg=7 ctermfg=0
hi CursorLineNr cterm=bold ctermbg=7 ctermfg=0
hi SignColumn ctermbg=7
hi GitGutterAdd ctermbg=7 ctermfg=2
hi GitGutterChange ctermbg=7 ctermfg=3
hi GitGutterDelete ctermbg=7 ctermfg=1
hi GitGutterChangeDelete ctermbg=7 ctermfg=3
hi Search ctermfg=0 ctermbg=7

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
endif

function! FoldColumnToggle()
  if &foldcolumn
    setlocal foldcolumn=0
  else
    setlocal foldcolumn=1
  endif
endfunction

function! LightlineFilename()
  return expand('%') !=# '' ? expand('%') : '[No Name]'
endfunction
