behave xterm
set nocompatible
set shiftwidth=2
set tabstop=2
set expandtab
set autoindent
set backspace=indent,eol,start
set foldcolumn=3
set foldminlines=2
set foldmethod=syntax
set foldenable
set completeopt=longest,menuone
set ruler
set showcmd
set spell spelllang=pl
set cursorline
set listchars=tab:-»,trail:·,eol:¬
set hlsearch
set incsearch
set nostartofline
set foldmarker=begin,end
set laststatus=1
set autochdir
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.rar,*.zip,*.tar.*,*.bmp,*.jpeg,*.avi,*.mov,*.mp3,*.ogg,*.flac
set nospell
set nonumber
set tags+=tags;
set mouse=a
set fo=tcrqn
set backup
set backupdir=~/.vim/backup

filetype plugin on
filetype indent on

syntax on

" Uzupełnianie Ruby'ego
autocmd FileType ruby,haml set omnifunc=rubycomplete#Complete

" Skróty
map <Leader>e :Ex<CR>
map <Leader>v :Vex<CR><C-w>=
map <F8> :set number!<CR>
map <F9> :set list!<CR>
map <F10> :set spell!<CR>
map <F11> :!ctags -R --exclude=.git --exclude=log --exclude=tmp * ~/.rvm/gems/ruby-head/* <CR>
map <F12> :Ex<CR>
cmap w!! %!sudo tee > /dev/null %

" Kolorki
colorscheme desert
hi DiffAdd gui=bold,undercurl guifg=palegreen guibg=Grey40
hi DiffChange gui=bold guifg=SkyBlue guibg=Grey40
hi DiffDelete gui=bold,undercurl guifg=indianred guibg=Grey40
hi DiffText gui=bold,undercurl guifg=SkyBlue guibg=Grey40

" Usuwanie trailing whitespaces
autocmd BufWritePre * :%s/\s\+$//e
