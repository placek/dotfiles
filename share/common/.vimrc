filetype plugin indent on
syntax enable

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
set formatoptions=tcrqn
set grepformat=%f:%l:%c:%m
set grepprg=ag\ --vimgrep\ $*
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
set showtabline=2
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

let mapleader = "\\"
let maplocalleader = ","

" mapping
nnoremap <leader>1  :set number!<CR>
nnoremap <leader>2  :set relativenumber!<CR>
nnoremap <leader>3  :set hlsearch!<CR>
nnoremap <leader>v  :vsplit<CR>
nnoremap <leader>s  :split<CR>
nnoremap <leader>\  :NERDTreeToggle<CR>
nnoremap <leader>/  :NERDTreeFind<CR>
nnoremap <leader>b  :Buffers<CR>
nnoremap <leader>B  :bufdo bd<CR>
nnoremap <leader>c  :terminal ++close ++rows=8<CR>
nnoremap <leader>f  :Ag<CR>
nnoremap <leader>F  :FZF<CR>
nnoremap <leader>gc :Commits<CR>
nnoremap <leader>gf :GFiles<CR>
nnoremap <leader>gs :GFiles?<CR>
nnoremap <leader>h  :History<CR>
nnoremap <leader>m  :Marks<CR>
nnoremap <leader>S  :Snippets<CR>
nnoremap <leader>t  :Tags<CR>
nnoremap <leader>T  :BTags<CR>

" coc
nmap <silent> [b :bprevious<CR>
nmap <silent> ]b :bnext<CR>
nmap <silent> [c <Plug>(coc-git-prevconflict)
nmap <silent> ]c <Plug>(coc-git-nextconflict)
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nmap <silent> [h <Plug>(coc-git-prevchunk)
nmap <silent> ]h <Plug>(coc-git-nextchunk)

xmap <silent> ic <Plug>(coc-classobj-i)
omap <silent> ic <Plug>(coc-classobj-i)
xmap <silent> ac <Plug>(coc-classobj-a)
omap <silent> ac <Plug>(coc-classobj-a)
xmap <silent> if <Plug>(coc-funcobj-i)
omap <silent> if <Plug>(coc-funcobj-i)
xmap <silent> af <Plug>(coc-funcobj-a)
omap <silent> af <Plug>(coc-funcobj-a)
omap <silent> ig <Plug>(coc-git-chunk-inner)
xmap <silent> ig <Plug>(coc-git-chunk-inner)
omap <silent> ag <Plug>(coc-git-chunk-outer)
xmap <silent> ag <Plug>(coc-git-chunk-outer)

nmap <localleader>,       :call <SID>show_documentation()<CR>
nmap <localleader>a       <Plug>(coc-codeaction-cursor)
nmap <localleader>b       :Gblame<CR>
nmap <localleader>c       <Plug>(coc-git-commit)
nmap <localleader>d       <Plug>(coc-definition)
nmap <localleader>e       <Plug>(coc-diagnostic-info)
nmap <localleader>f       <Plug>(coc-format)
vmap <localleader>f       <Plug>(coc-format-selected)
nmap <localleader>h       <Plug>(coc-git-chunkinfo)
nmap <localleader>i       <Plug>(coc-implementation)
nmap <localleader>n       <Plug>(coc-rename)
nmap <localleader>q       <Plug>(coc-refactor)
nmap <localleader>r       <Plug>(coc-references)
nmap <localleader>y       <Plug>(coc-type-definition)
nmap <localleader><space> :<C-u>CocFzfList<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')

command! -nargs=0 Format :call CocAction('format')
let g:coc_global_extensions = ['coc-tag', 'coc-git']

" options
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeDirArrows = 1
let NERDTreeMinimalUI = 1
let NERDTreeWinSize = 32
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline_powerline_fonts = 1
let g:airline_theme = 'base16_colors'
let g:fzf_tags_command = 'git ctags'
let g:nerdtree_tabs_autoclose = 0

" search selection
function! s:getSelectedText()
  let l:old_reg = getreg('"')
  let l:old_regtype = getregtype('"')
  norm gvy
  let l:ret = getreg('"')
  call setreg('"', l:old_reg, l:old_regtype)
  exe "norm \<Esc>"
  return l:ret
endfunction

vnoremap <silent> * :call setreg("/", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent> # :call setreg("?", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent> f :<C-u>call fzf#vim#ag(<SID>getSelectedText())<CR>
vnoremap <silent> t :<C-u>call fzf#vim#tags(<SID>getSelectedText())<CR>

function! s:fzfNERDTreeNode()
  call NERDTreeCopyPath()
  let l:path = getreg('+')
  call fzf#run(fzf#wrap({'source': 'git ls-files ' . l:path}))
endfunction

" autocommands
autocmd BufWritePre * :%s/\s\+$//e
autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
autocmd FileType make setlocal noexpandtab
autocmd FileType haskell setlocal makeprg=cabal\ build
autocmd FileType nerdtree :vert resize 32
autocmd FileType nerdtree nnoremap <leader>gf :call <SID>fzfNERDTreeNode()<CR>
autocmd FileType ruby
  \ if expand("%") =~# '_spec\.rb$' |
  \   compiler rspec | setl makeprg=rspec\ --no-color\ % |
  \ else |
  \   setl makeprg=rubocop\ --format\ clang\ % |
  \ endif
autocmd! FileType fzf set laststatus=0 noshowmode noruler | autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" commands
command! MakeTags !git ctags
command! Open !open %

" colors
hi CocGitAddedSign         ctermbg=0 ctermfg=2
hi CocGitChangeRemovedSign ctermbg=0 ctermfg=3
hi CocGitChangedSign       ctermbg=0 ctermfg=3
hi CocGitRemovedSign       ctermbg=0 ctermfg=1
hi CocGitTopRemovedSign    ctermbg=0 ctermfg=1
hi CocHighlightText        ctermbg=18 ctermfg=2
hi ColorColumn             ctermbg=18
hi DiffAdd                 ctermbg=2 ctermfg=0 cterm=BOLD
hi DiffChange              ctermbg=3 ctermfg=0 cterm=BOLD
hi DiffDelete              ctermbg=1 ctermfg=0 cterm=BOLD
hi DiffText                ctermbg=2 ctermfg=0 cterm=BOLD
hi Directory               ctermfg=blue
hi FoldColumn              ctermbg=0 ctermfg=7
hi Folded                  ctermbg=6 ctermfg=0
hi Pmenu                   ctermbg=8
hi Search                  ctermbg=2 ctermfg=0
hi SignColumn              ctermbg=0
hi Visual                  ctermbg=7 ctermfg=0

" FZF extension
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = { 'ctrl-q': function('s:build_quickfix_list'), 'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit' }
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
let $FZF_DEFAULT_COMMAND = 'git ls-files'
