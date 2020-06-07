" plugins
call plug#begin('~/.vim/bundle')
  Plug 'airblade/vim-gitgutter'
  Plug 'edkolev/tmuxline.vim'
  Plug 'godlygeek/tabular'
  Plug 'jistr/vim-nerdtree-tabs'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
  Plug 'scrooloose/nerdtree'
  Plug 'tpope/vim-fugitive'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'xuyuanp/nerdtree-git-plugin'
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
set path+=**
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
set ttyfast
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.rar,*.zip,*.tar.*,*.bmp,*.jpeg,*.avi,*.mov,*.mp7,*.ogg,*.flac
set wrapmargin=0
set wildmenu

map  <ESC>[H <Home>
map  <ESC>[F <End>
imap <ESC>[H <Home>
imap <ESC>[F <End>

" mapping
nnoremap <Leader>v :vsplit<CR>
nnoremap <Leader>s :split<CR>
nnoremap <Leader>1 :set number!<CR>
nnoremap <Leader>2 :set relativenumber!<CR>
nnoremap <Leader>3 :GitGutterToggle<CR>
nnoremap <Leader>4 :set hlsearch!<CR>
nnoremap <Leader>5 :set list!<CR>
nnoremap <Leader>\ :NERDTreeToggle<CR>
nnoremap <C-\>     :NERDTreeFind<CR>
nnoremap <Leader>f :Ag<CR>
nnoremap <Leader>F :FZF<CR>
nnoremap <Leader>t :Tags<CR>
nnoremap <Leader>T :BTags<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>B :bufdo bd<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>l :Lines<CR>
nnoremap <Leader>L :Blines<CR>
nnoremap <Leader>C :Commits<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>G :GFiles?<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>q *``cgn
vnoremap <Leader>q "qy/<C-R>q<CR>``cgn
nnoremap <Leader>r :split %:s?app/?spec/?:s?.rb?_spec.rb?<CR>
nnoremap <Leader>R :split %:s?spec/?app/?:s?_spec.rb?.rb?<CR>
nnoremap <Leader>o o<esc>
nnoremap <Leader>O O<esc>
nnoremap <silent>* *``
nnoremap <silent># #``
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
vnoremap // y/<C-R>"<CR>
vnoremap <Leader>a{ :Tabularize /^[^{]*/<CR>
vnoremap <Leader>a= :Tabularize /^[^=]*/<CR>
vnoremap <Leader>a: :Tabularize /:\zs/<CR>

" options
let g:fzf_tags_command = 'git ctags'
let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:nerdtree_tabs_autoclose = 0
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=2  " open in vertical split
let g:netrw_winsize = 15    " netrw 15% of window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" autocommands
augroup normalize
  autocmd BufWritePre * :%s/\s\+$//e
  autocmd BufRead * normal zR
augroup END

autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
autocmd FileType make setlocal noexpandtab
autocmd FileType haskell setlocal makeprg=cabal\ build

command! MakeTags !git ctags

" colors
hi SignColumn ctermbg=7
hi Search cterm=none ctermfg=3 ctermbg=7
hi Pmenu ctermfg=0 ctermbg=7
hi PmenuSel ctermfg=0 ctermbg=7 cterm=bold
hi GitGutterAdd ctermbg=7 ctermfg=2
hi GitGutterChange ctermbg=7 ctermfg=3
hi GitGutterChangeDelete ctermbg=7 ctermfg=3
hi GitGutterDelete ctermbg=7 ctermfg=1

" FZF extension
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit' }

let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
