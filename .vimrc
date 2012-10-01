behave xterm
set nocompatible
set shiftwidth=2
set tabstop=2
set expandtab
set autoindent
set backspace=indent,eol,start
set foldcolumn=2
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
set ofu=syntaxcomplete#Complete

filetype plugin on
filetype indent on

syntax on

" Ruby
autocmd FileType ruby,haml let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,haml let g:rubycomplete_rails = 1
autocmd FileType ruby,haml let g:rubycomplete_classes_in_global = 1

" Skróty
map <Leader>e :Ex<CR>
map <Leader>v :Vex<CR><C-w>=
map <Leader>s :Sex<CR><C-w>=
map <Leader>d :DiffChangesDiffToggle<CR>
map <Leader>m :TagbarToggle<CR>
map <Leader>1 :set number!<CR>
map <Leader>2 :set list!<CR>
map <Leader>3 :set spell!<CR>
map <Leader>4 :!ctags -R --exclude=.git --exclude=log --exclude=tmp * ~/.rvm/gems/ruby-head <CR>
cmap w!! %!sudo tee > /dev/null %
cmap W w
cmap Wq wq
cmap Wall wall
cmap Q q
cmap Q! q!
cmap Qall qall
cmap Qall! qall!
vmap Y "*y

" Kolorki
colorscheme desert
hi DiffAdd term=reverse cterm=bold ctermbg=green ctermfg=white
hi DiffChange term=reverse cterm=bold ctermbg=gray ctermfg=black
hi DiffText term=reverse cterm=bold ctermbg=blue ctermfg=black
hi DiffDelete term=reverse cterm=bold ctermbg=red ctermfg=black

" Autocomplete menu
hi Pmenu ctermbg=238 gui=bold

" Usuwanie trailing whitespaces
autocmd BufWritePre * :%s/\s\+$//e
