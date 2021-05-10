filetype plugin indent on
syntax enable

" settings
set backspace=indent,eol,start
set clipboard=unnamedplus
set cmdheight=2
set completeopt=longest,menuone
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
set timeoutlen=1000 ttimeoutlen=0
set ttyfast
set updatetime=300
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.rar,*.zip,*.tar.*,*.bmp,*.jpeg,*.avi,*.mov,*.mp7,*.ogg,*.flac
set wildmenu
set wrapmargin=0

" mapping
nnoremap <Leader>v :vsplit<CR>
nnoremap <Leader>s :split<CR>
nnoremap <Leader>1 :set number!<CR>
nnoremap <Leader>2 :set relativenumber!<CR>
nnoremap <Leader>3 :GitGutterToggle<CR>
nnoremap <Leader>4 :set hlsearch!<CR>
nnoremap <Leader>\ :NERDTreeToggle<CR>
nnoremap <Leader>/ :NERDTreeFind<CR>
nnoremap <Leader>f :Ag<CR>
vnoremap <Leader>f :call <SID>FzfSelectedWord()<CR>
nnoremap <Leader>F :FZF<CR>
nnoremap <Leader>t :call <SID>FzfTagsCurrentWord()<CR>
nnoremap <Leader>T :BTags<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>B :bufdo bd<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>gc :Commits<CR>
nnoremap <Leader>gf :GFiles<CR>
nnoremap <Leader>gF :GFiles?<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>q :set opfunc=<SID>SearchOperator<CR>g@
vnoremap <Leader>q :<C-u>call <SID>SearchOperator(visualmode())<CR>
nnoremap <Leader>Q :set opfunc=<SID>SearchProjectOperator<CR>g@
vnoremap <Leader>Q :<C-u>call <SID>SearchProjectOperator(visualmode())<CR>
nnoremap <Leader>S :Snippets<CR>
nnoremap <Leader>C :call <SID>OpenInTerminal()<CR>
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
vnoremap // y/<C-R>"<CR>
vnoremap <Leader>a{ :Tabularize /^[^{]*/<CR>
vnoremap <Leader>a= :Tabularize /^[^=]*/<CR>
vnoremap <Leader>a: :Tabularize /:/<CR>
vnoremap <Leader>A: :Tabularize /:\zs/<CR>

" coc
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nmap <silent> [h <Plug>(coc-git-prevchunk)
nmap <silent> ]h <Plug>(coc-git-nextchunk)
nmap <silent> [c <Plug>(coc-git-prevconflict)
nmap <silent> ]c <Plug>(coc-git-nextconflict)
nmap <silent> gs <Plug>(coc-git-chunkinfo)
nmap <silent> gc <Plug>(coc-git-commit)
omap <silent> ig <Plug>(coc-git-chunk-inner)
xmap <silent> ig <Plug>(coc-git-chunk-inner)
omap <silent> ag <Plug>(coc-git-chunk-outer)
xmap <silent> ag <Plug>(coc-git-chunk-outer)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent><nowait> <space>a <Plug>(coc-codeaction)
nmap <silent><nowait> <space>r <Plug>(coc-rename)
nnoremap <silent><nowait> <space>d :call <SID>show_documentation()<CR>
nnoremap <silent><nowait> <space>e :<C-u>CocList extensions<CR>
nnoremap <silent><nowait> <space>c :<C-u>CocList commands<CR>
nnoremap <silent><nowait> <space>g :<C-u>CocList diagnostics<CR>
nnoremap <silent><nowait> <space>o :<C-u>CocList outline<CR>
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

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
let g:fzf_tags_command = 'git ctags'
let g:airline_theme = 'base16_colors'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:nerdtree_tabs_autoclose = 0
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeWinSize = 32

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

" FZF-search visual-selected word
function! g:GetVisualSelectionText()
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  let lines = getline(line_start, line_end)
  if len(lines) == 0
    return ""
  endif
  let lines[-1] = lines[-1][: column_end - 2]
  let lines[0] = lines[0][column_start - 1:]
  return join(lines, "\n")
endfunction

function! s:FzfSelectedWord()
  let l:word = GetVisualSelectionText()
  call fzf#vim#ag(l:word, fzf#vim#with_preview())
endfunction

" FZF-search tags with word under a cursor
function! s:FzfTagsCurrentWord()
  let l:word = expand('<cword>')
  let l:list = taglist(l:word)
  if len(l:list) == 1
    call fzf#vim#tags("")
  else
    call fzf#vim#tags(l:word)
  endif
endfunction

" open terminal with proper command
function! s:OpenInTerminal()
  try
    silent exec "terminal ++close ++rows=8 ". b:termprg
  catch
    silent exec "terminal ++close ++rows=8"
  echo
  endtry
endfunction

" autocommands
autocmd BufWritePre * :%s/\s\+$//e
autocmd BufRead * normal zR
autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
autocmd FileType make setlocal noexpandtab
autocmd FileType haskell setlocal makeprg=cabal\ build
autocmd FileType haskell let b:termprg="ghci %:p"
autocmd FileType nerdtree :vert resize 32
autocmd FileType ruby let b:termprg="irb -r %:p"
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
hi SignColumn              ctermbg=0
hi FoldColumn              ctermbg=0 ctermfg=7
hi Folded                  ctermbg=6 ctermfg=0
hi CocGitAddedSign         ctermbg=0 ctermfg=2
hi CocGitRemovedSign       ctermbg=0 ctermfg=1
hi CocGitTopRemovedSign    ctermbg=0 ctermfg=1
hi CocGitChangedSign       ctermbg=0 ctermfg=3
hi CocGitChangeRemovedSign ctermbg=0 ctermfg=3
hi DiffAdd                 ctermbg=2 ctermfg=0 cterm=BOLD
hi DiffDelete              ctermbg=1 ctermfg=0 cterm=BOLD
hi DiffChange              ctermbg=3 ctermfg=0 cterm=BOLD
hi DiffText                ctermbg=2 ctermfg=0 cterm=BOLD
hi Visual                  ctermbg=7 ctermfg=0
hi Search                  ctermbg=2 ctermfg=0
hi Directory               ctermfg=blue
hi Pmenu                   ctermbg=8
hi CocHighlightText        ctermbg=18 ctermfg=2

" FZF extension
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = { 'ctrl-q': function('s:build_quickfix_list'), 'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit' }
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
let $FZF_DEFAULT_COMMAND = 'git ls-files'
