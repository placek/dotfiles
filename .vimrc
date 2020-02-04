" plugins
call plug#begin('~/.vim/bundle')
  Plug 'airblade/vim-gitgutter'
  Plug 'edkolev/tmuxline.vim'
  Plug 'godlygeek/tabular'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
  Plug 'mattn/emmet-vim'
  Plug 'mbbill/undotree'
  Plug 'scrooloose/nerdtree'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-surround'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'xuyuanp/nerdtree-git-plugin'
  Plug 'jistr/vim-nerdtree-tabs'
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
nnoremap <Leader>\ :NERDTreeTabsToggle<CR>
nnoremap <Leader>1 :set relativenumber!<CR>
nnoremap <Leader>2 :GitGutterToggle<CR>
nnoremap <Leader>3 :set hlsearch!<CR>
nnoremap <Leader>4 :set list!<CR>
nnoremap <Leader>u :UndotreeToggle<CR>
nnoremap <Leader>f :Ag<CR>
nnoremap <Leader>F :FZF<CR>
nnoremap <Leader>t :Tags<CR>
nnoremap <Leader>T :BTags<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>B :bufdo bd<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>C :Commits<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>G :GFiles?<CR>
nnoremap <Leader>q *``cgn
nnoremap <Leader>r :NERDTreeFind<CR>
nnoremap <Leader>o o<esc>
nnoremap <Leader>O O<esc>
nnoremap <silent>* *``
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
vnoremap // y/<C-R>"<CR>
vnoremap <Leader>a{ :Tabularize /^[^{]*/<CR>

" options
let g:fzf_tags_command = 'git ctags'
let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

" autocommands
augroup normalize
  autocmd BufWritePre * :%s/\s\+$//e
  autocmd BufRead * normal zR
augroup END

autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
autocmd FileType make setlocal noexpandtab
