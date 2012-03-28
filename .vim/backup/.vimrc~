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
map <Leader>1 :set number!<CR>
map <Leader>2 :set list!<CR>
map <Leader>3 :set spell!<CR>
map <Leader>4 :!ctags -R --exclude=.git --exclude=log --exclude=tmp * /usr/local/rvm/gems/ruby-head <CR>
cmap w!! %!sudo tee > /dev/null %
cmap W w
cmap Wq wq
cmap Wall wall
cmap Q q
cmap Q! q!
cmap Qall qall
cmap Qall! qall!
vmap Y "+y
imap . .<C-p>
map <C-m> <C-W>>
map <C-n> <C-W><

" Kolorki
colorscheme desert
hi DiffAdd term=reverse cterm=bold ctermbg=green ctermfg=white
hi DiffChange term=reverse cterm=bold ctermbg=gray ctermfg=black
hi DiffText term=reverse cterm=bold ctermbg=blue ctermfg=black
hi DiffDelete term=reverse cterm=bold ctermbg=red ctermfg=black

" Usuwanie trailing whitespaces
autocmd BufWritePre * :%s/\s\+$//e
