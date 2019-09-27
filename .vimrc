call plug#begin('~/.vim/bundle')
  Plug 'airblade/vim-gitgutter'
  Plug 'edkolev/tmuxline.vim'
  Plug 'godlygeek/tabular'
  Plug 'jiangmiao/auto-pairs'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
  Plug 'mattn/emmet-vim'
  Plug 'scrooloose/nerdtree'
  Plug 'terryma/vim-multiple-cursors'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-surround'
  Plug 'xuyuanp/nerdtree-git-plugin'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
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
set foldmethod=indent
set formatoptions=tcrqn
set hlsearch
set incsearch
set laststatus=2
set listchars=tab:→\ ,space:·,eol:¬,nbsp:◦
set mouse=a
set nocompatible
set noshowmode
set nospell
set omnifunc=syntaxcomplete#Complete
set runtimepath+=/usr/local/opt/fzf
set shiftwidth=2
set showcmd
set showtabline=2
set softtabstop=2
set splitbelow
set splitright
set swapfile
set tabstop=2
set tags+=.git/tags;
set term=xterm-256color
set timeoutlen=1000 ttimeoutlen=0
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
nnoremap <silent> <2-LeftMouse> *``

if exists(":Tabularize")
  vnoremap ,{ :Tabularize /^[^{]*/<CR>
endif

" options
let g:fzf_tags_command = 'git ctags'
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_theme='solarized'

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

augroup jb_file
  autocmd BufNewFile, BufRead *.jb set syntax=ruby
augroup END

autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
autocmd FileType make setlocal noexpandtab

" colors
hi CursorLineNr cterm=bold ctermbg=7 ctermfg=0
hi FoldColumn ctermbg=7 ctermfg=0
hi GitGutterAdd ctermbg=7 ctermfg=2
hi GitGutterChange ctermbg=7 ctermfg=3
hi GitGutterChangeDelete ctermbg=7 ctermfg=3
hi GitGutterDelete ctermbg=7 ctermfg=1
hi LineNr ctermbg=7 ctermfg=0
hi Search ctermfg=0 ctermbg=7
hi SignColumn ctermbg=7
hi DiffAdd ctermfg=7 ctermbg=2
hi DiffDelete ctermfg=7 ctermbg=1
hi DiffChange ctermfg=7 ctermbg=4
hi DiffText ctermfg=7

function! FoldColumnToggle()
  if &foldcolumn
    setlocal foldcolumn=0
  else
    setlocal foldcolumn=1
  endif
endfunction
