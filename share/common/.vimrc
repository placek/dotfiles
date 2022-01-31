filetype plugin indent on
syntax enable

" functions
function! GetSelectedText()
  norm gv"sy
  let l:ret = getreg('s')
  exe "norm \<Esc>"
  return l:ret
endfunction

function! MakeTags()
  let tags_job = job_start("git ctags", #{ exit_cb: function('MakeTagsResult') })
endfunction

function! MakeFolds()
  setlocal foldmethod=indent
  norm zR
  setlocal foldmethod=manual
endfunction

function! MakeTagsResult(job, status)
  if a:status == 0
    echom "MakeTags: done"
  else
    echom "MakeTags: tags generation failed"
  endif
endfunction

" settings
set backspace=indent,eol,start
set clipboard=unnamedplus
set cmdheight=1
set colorcolumn=80,160
set completeopt=menu,menuone,noselect
set cursorline
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
set diffopt-=internal
set dir=/tmp
set encoding=utf-8
set encoding=utf-8
set expandtab
set exrc
set foldcolumn=1
set grepformat=%f:%l:%c:%m
set grepprg=rg\ --vimgrep\ $*
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
set noswapfile
set nowritebackup
set number
set path+=**
set shiftwidth=2
set shortmess+=c
set showcmd
set signcolumn=yes
set softtabstop=2
set splitright
set tabstop=2
set tags+=.git/tags;
set termencoding=utf-8
set timeoutlen=1000 ttimeoutlen=0
set ttyfast
set updatetime=300
set wildignore=.*,.git/
set wildmenu

" options
let g:mapleader = "\\"
let g:maplocalleader = ","

" mappings
nnoremap <leader>1  :set number!<CR>
nnoremap <leader>2  :set hlsearch!<CR>
nnoremap <leader>3  :call MakeFolds()<CR>
nnoremap <leader>4  :set spell!<CR>

nmap <silent>       [a :previous<CR>
nmap <silent>       ]a :next<CR>
nmap <silent>       [b :bprevious<CR>
nmap <silent>       ]b :bnext<CR>
nmap <silent>       [w <C-w>W
nmap <silent>       ]w <C-w>w
nmap <silent>       [t :tabprevious<CR>
nmap <silent>       ]t :tabnext<CR>
nmap <silent>       [e :cprevious<CR>
nmap <silent>       ]e :cnext<CR>
nmap <silent>       [l :lprevious<CR>
nmap <silent>       ]l :lnext<CR>

vnoremap <silent>*  :<C-u>call setreg("/", substitute(GetSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent>#  :<C-u>call setreg("?", substitute(GetSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vmap     v          <Plug>(expand_region_expand)
vmap     <C-v>      <Plug>(expand_region_shrink)

" commands
command! -count MakeTags call MakeTags()
command! -count Open     !open %

" colors
hi ColorColumn                      ctermbg=18
hi DiffAdd                          ctermbg=2  ctermfg=0 cterm=BOLD
hi DiffChange                       ctermbg=3  ctermfg=0 cterm=BOLD
hi DiffDelete                       ctermbg=1  ctermfg=0 cterm=BOLD
hi DiffText                         ctermbg=2  ctermfg=0 cterm=BOLD
hi Directory                                   ctermfg=4
hi GitSignsCurrentLineBlame                    ctermfg=8
hi GitSignsAdd                      ctermbg=0  ctermfg=2
hi GitSignsChange                   ctermbg=0  ctermfg=3
hi GitSignsDelete                   ctermbg=0  ctermfg=1
hi FoldColumn                       ctermbg=0  ctermfg=7
hi Folded                           ctermbg=19 ctermfg=7
hi LspDiagnosticsSignError          ctermbg=0  ctermfg=1
hi LspDiagnosticsSignHint           ctermbg=0  ctermfg=7
hi LspDiagnosticsSignInfo           ctermbg=0  ctermfg=4
hi LspDiagnosticsSignWarning        ctermbg=0  ctermfg=16
hi LspDiagnosticsVirtualTextError   ctermbg=0  ctermfg=1
hi LspDiagnosticsVirtualTextHint    ctermbg=0  ctermfg=7
hi LspDiagnosticsVirtualTextInfo    ctermbg=0  ctermfg=4
hi LspDiagnosticsVirtualTextWarning ctermbg=0  ctermfg=16
hi LspReferenceRead                 ctermbg=18 ctermfg=4
hi LspReferenceWrite                ctermbg=18 ctermfg=11
hi MatchParen                       ctermbg=18 ctermfg=2
hi NormalFloat                      ctermbg=18 ctermfg=7
hi Pmenu                            ctermbg=0  ctermfg=8
hi PmenuSbar                        ctermbg=0  ctermfg=8
hi PmenuSel                         ctermbg=18 ctermfg=7
hi Search                           ctermbg=2  ctermfg=0
hi SignColumn                       ctermbg=0
hi TabLine                          ctermbg=0  ctermfg=7 cterm=NONE
hi TabLineFill                      ctermbg=0  ctermfg=0
hi TabLineSel                       ctermbg=0  ctermfg=9 cterm=NONE
hi VertSplit                        ctermbg=8  ctermfg=8
hi Visual                           ctermbg=7  ctermfg=0

" autocommands
autocmd! BufWritePost * :silent! MakeTags
autocmd! BufWritePre * :%s/\s\+$//e
