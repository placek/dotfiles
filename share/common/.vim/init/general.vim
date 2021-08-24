" functions
function! s:getSelectedText()
  norm gv"sy
  let l:ret = getreg('s')
  exe "norm \<Esc>"
  return l:ret
endfunction

function! s:gitBlame(bufnr, filename, ...)
  execute 'leftabove 40 vnew'
  execute 'autocmd BufWipeout <buffer> call setbufvar(' . a:bufnr .', "&cursorbind", 0)'
  execute 'read!git blame --date short --minimal ' . shellescape(a:filename)
  set buftype=nofile bufhidden=wipe nowrap noswapfile nonumber norelativenumber cursorbind nowrap foldcolumn=0 nofoldenable winfixwidth filetype=git
  0delete _
  wincmd p
  set cursorbind
endfunction

function! s:searchWithVimgrep(query, files = "**/*")
  if len(a:query) != 0
    execute "lvimgrep /".escape(a:query, '^$.*?/\[]')."/g ".a:files
  endif
endfunction

function! StatusLineMode()
  let l:mode=mode()
  if l:mode==#"n"
    return "  NORMAL "
  elseif l:mode==?"v"
    return "  VISUAL "
  elseif l:mode==#""
    return "  VISUAL BLK "
  elseif l:mode==#"i"
    return "  INSERT "
  elseif l:mode==#"t"
    return "  TERMINAL "
  elseif l:mode==#"R"
    return "  REPLACE "
  endif
endfunction

function! MakeTagsResult(job, status)
  if a:status == 0
    echom "MakeTags: done"
  else
    echom "MakeTags: tags generation failed"
  endif
endfunction

function! s:makeTags()
  let tags_job = job_start("git ctags", #{ exit_cb: function('MakeTagsResult') })
endfunction

function! CleverTab()
  if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
    return "\<Tab>"
  elseif exists(":ALEInfo")
    return "\<C-X>\<C-O>"
  else
    return "\<C-N>"
  endif
endfunction

function! s:placeComment()
  let l:comment_pattern = substitute(escape(&commentstring, '^$.*?/\[]'), '%s', "\\&", '')
  execute 's/\%V.*/'.l:comment_pattern
endfunction

" settings
set backspace=indent,eol,start
set clipboard=unnamedplus
set cmdheight=1
set colorcolumn=80,160
set cursorline
set dir=/tmp
set encoding=utf-8
set expandtab
set foldcolumn=1
set foldmethod=manual
set grepformat=%f:%l:%c:%m
set grepprg=rg\ --vimgrep\ $*
set laststatus=2
set list
set listchars=tab:»\ ,nbsp:␣,trail:·,extends:›,precedes:‹
set mouse=a
set noshowmode
set nospell
set number
set relativenumber
set shiftwidth=2
set shortmess=a
set showcmd
set softtabstop=2
set swapfile
set tabstop=2
set termencoding=utf-8
set timeoutlen=1000 ttimeoutlen=0
set ttyfast
set updatetime=300
set wildignore=.*,.git/

" status
set statusline=
set statusline+=%#StatusLineMode#
set statusline+=%{StatusLineMode()}
set statusline+=%#StatusLineInfo#
set statusline+=\ %n
set statusline+=%#StatusLine#
set statusline+=\ %f:%l:%c
set statusline+=%#StatusLineInfo#%=
set statusline+=\ %m
set statusline+=\ %y
set statusline+=\ \[%{&fileencoding?&fileencoding:&encoding}\]
set statusline+=\ \[%{&fileformat}\]
set statusline+=\ \[%p%%\ %L\]

" options
let g:mapleader = "\\"
let g:maplocalleader = ","
let g:netrw_altv = 1
let g:netrw_preview = 1

" mappings
nnoremap <C-\><C-\> :Lexplore<CR>
nnoremap <leader>/  :ExploreFind<CR>
nnoremap <leader>\  :Explore<CR>
nnoremap <leader>1  :set number!<CR>
nnoremap <leader>2  :set relativenumber!<CR>
nnoremap <leader>3  :set hlsearch!<CR>

nnoremap <leader>b  :ls<CR>
nnoremap <leader>c  :terminal ++close ++rows=10<CR>
nnoremap <leader>f  :call <SID>searchWithVimgrep(input("/"))<CR>
nnoremap <leader>F  :find
nnoremap <leader>gc :!git l<CR>
nnoremap <leader>gf :!git ls-files<CR>
nnoremap <leader>gs :!git st<CR>
nnoremap <leader>h  :jumps<CR>
nnoremap <leader>m  :marks<CR>
nnoremap <leader>s  :set cursorbind!<CR>

nmap <silent>       [b :bprevious<CR>
nmap <silent>       ]b :bnext<CR>
nmap <silent>       [t :tabprevious<CR>
nmap <silent>       ]t :tabnext<CR>
nmap <silent>       [w <C-w>W
nmap <silent>       ]w <C-w>w

nmap <localleader>b :Blame<CR>

vnoremap <leader>F  :<C-u>call <SID>searchWithVimgrep(<SID>getSelectedText())<CR>
vnoremap <silent>*  :call setreg("/", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent>#  :call setreg("?", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent>g/ :call <SID>placeComment()<CR>

inoremap <Tab>      <C-R>=CleverTab()<CR>

" commands
command! -count Blame call <SID>gitBlame(bufnr('%'), expand('%:p'), <f-args>)
command! MakeTags     call <SID>makeTags()
command! Open         !open %
command! ExploreFind  let @/=expand("%:t") | execute 'Explore' expand("%:h") | normal n

" colors
hi clear StatusLineNC
hi ColorColumn      ctermbg=18
hi DiffAdd          ctermbg=2 ctermfg=0 cterm=BOLD
hi DiffChange       ctermbg=3 ctermfg=0 cterm=BOLD
hi DiffDelete       ctermbg=1 ctermfg=0 cterm=BOLD
hi DiffText         ctermbg=2 ctermfg=0 cterm=BOLD
hi Directory        ctermfg=4
hi FoldColumn       ctermbg=0 ctermfg=7
hi Folded           ctermbg=6 ctermfg=0
hi Pmenu            ctermbg=8
hi Search           ctermbg=2 ctermfg=0
hi SignColumn       ctermbg=0
hi StatusLineMode   ctermbg=16 ctermfg=0
hi StatusLine       ctermbg=0 ctermfg=7 cterm=NONE
hi StatusLineInfo   ctermbg=0 ctermfg=8 cterm=BOLD
hi TabLine          ctermbg=0 ctermfg=7 cterm=NONE
hi TabLineFill      ctermbg=0 ctermfg=0
hi TabLineSel       ctermbg=0 ctermfg=9 cterm=NONE
hi VertSplit        ctermbg=8 ctermfg=8
hi Visual           ctermbg=7 ctermfg=0
hi netrwComment     ctermfg=8
hi netrwTreeBar     ctermfg=8

" autocommands
autocmd! BufWritePost * :silent! MakeTags
autocmd! BufWritePre * :%s/\s\+$//e
autocmd! FileType git nnoremap yy 0viwy
autocmd! FileType make setlocal noexpandtab
autocmd! FileType haskell packadd haskell-vim | syntax on
