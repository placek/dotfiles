filetype plugin indent on
syntax enable

" plugins
packadd! matchit
packadd! fzf
packadd! fzf_vim
packadd! nerdtree
packadd! nerdtree_git_plugin
packadd! vim_airline
packadd! vim_airline_themes
packadd! vim_gitgutter
packadd! tmuxline
packadd! tabular
packadd! syntastic
packadd! fugitive

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
set grepformat=%f:%l:%c:%m
set grepprg=ag\ --vimgrep\ $*
set hlsearch
set incsearch
set laststatus=2
set listchars=tab:→\ ,space:·,eol:¬,nbsp:◦
set mouse=a
set nocompatible
set noshowmode
set nospell
set number
set omnifunc=syntaxcomplete#Complete
set path+=**
set relativenumber
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
nnoremap <Leader>/ :NERDTreeFind<CR>
nnoremap <Leader>f :Ag<CR>
nnoremap <Leader>F :FZF<CR>
nnoremap <Leader>t :Tags<CR>
nnoremap <Leader>T :BTags<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>B :bufdo bd<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>l :Lines<CR>
nnoremap <Leader>L :BLines<CR>
nnoremap <Leader>C :Commits<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>G :GFiles?<CR>
nnoremap <Leader>Gb :Gblame<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>q :set opfunc=<SID>SearchOperator<CR>g@
vnoremap <Leader>q :<C-u>call <SID>SearchOperator(visualmode())<CR>
nnoremap <Leader>Q :set opfunc=<SID>SearchProjectOperator<CR>g@
vnoremap <Leader>Q :<C-u>call <SID>SearchProjectOperator(visualmode())<CR>
nnoremap <Leader>r :split %:s?app/?spec/?:s?.rb?_spec.rb?<CR>
nnoremap <Leader>R :split %:s?spec/?app/?:s?_spec.rb?.rb?<CR>
nnoremap <Leader>o o<esc>
nnoremap <Leader>O O<esc>
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
vnoremap // y/<C-R>"<CR>
vnoremap <Leader>a{ :Tabularize /^[^{]*/<CR>
vnoremap <Leader>a= :Tabularize /^[^=]*/<CR>
vnoremap <Leader>a: :Tabularize /:/<CR>
vnoremap <Leader>A: :Tabularize /:\zs/<CR>

" options
let g:fzf_tags_command = 'git ctags'
let g:airline_theme = 'base16_atelierdune'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#ale#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:nerdtree_tabs_autoclose = 0
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeWinSize = 32
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 20

" search operator
function! s:SearchOperator(type)
  if a:type ==# 'v'
    silent exec "normal! `<v`>\"ry"
  elseif a:type ==# 'char'
    silent exec "normal! `[v`]\"ry"
  else
    return
  endif
  let @/=@r
endfunction

function! s:SearchProjectOperator(type)
  if a:type ==# 'v'
    silent exec "normal! `<v`>\"ry"
  elseif a:type ==# 'char'
    silent exec "normal! `[v`]\"ry"
  else
    return
  endif
  silent exec "vimgrep! /" . shellescape(@@) . "/gj **/*.rb"
endfunction

" autocommands
autocmd BufWritePre * :%s/\s\+$//e
autocmd BufRead * normal zR
autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
autocmd FileType make setlocal noexpandtab
autocmd FileType haskell setlocal makeprg=cabal\ build
autocmd FileType nerdtree :vert resize 32
autocmd FileType ruby
  \ if expand("%") =~# '_spec\.rb$' |
  \   compiler rspec | setl makeprg=rspec\ --no-color\ % |
  \ else |
  \   setl makeprg=rubocop\ --format\ clang\ % |
  \ endif

command! MakeTags !git ctags
command! Open !open %

" colors
hi SignColumn ctermbg=7
hi Search cterm=none ctermfg=3 ctermbg=7
hi Pmenu ctermfg=0 ctermbg=7
hi PmenuSel ctermfg=0 ctermbg=7 cterm=bold
hi GitGutterAdd ctermbg=7 ctermfg=2
hi GitGutterChange ctermbg=7 ctermfg=3
hi GitGutterChangeDelete ctermbg=7 ctermfg=3
hi GitGutterDelete ctermbg=7 ctermfg=1
hi LineNr ctermfg=14
hi CursorLineNr ctermfg=14

" FZF extension
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = { 'ctrl-q': function('s:build_quickfix_list'), 'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit' }
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
