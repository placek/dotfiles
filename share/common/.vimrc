filetype plugin indent on
syntax enable

" functions
function! GetSelectedText()
  norm gv"sy
  let l:ret = getreg('s')
  exe "norm \<Esc>"
  return l:ret
endfunction

function! MakeFolds()
  setlocal foldmethod=indent
  norm zR
  setlocal foldmethod=manual
endfunction

function! MakeTags()
  call jobstart("git ctags", { "on_exit": function('MakeTagsResult') })
endfunction

function! MakeTagsResult(job, status, event)
  if a:status == 0
    echom "MakeTags: done"
  else
    echom "MakeTags: tags generation failed"
  endif
endfunction

" settings
set autowrite
set backspace=indent,eol,start
set clipboard=unnamedplus
set cmdheight=1
set colorcolumn=80,160
set completeopt=menu,menuone,noselect
set cursorline
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
set diffopt-=internal
set encoding=utf-8
set expandtab
set foldcolumn=1
set foldmethod=manual
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
set number
set path+=**
set scrolloff=4
set shiftwidth=2
set shortmess+=c
set showcmd
set signcolumn=yes
set softtabstop=2
set splitbelow
set splitright
set tabstop=2
set tags+=.git/tags;
set termencoding=utf-8
set timeoutlen=1000 ttimeoutlen=0
set ttyfast
set undodir=/tmp/undodir
set undofile
set updatetime=300
set wildignore=.*,.git/
set wildmenu

" options
let g:mapleader      = "\\"
let g:maplocalleader = ","

" mappings
nnoremap <leader>1 :set relativenumber!<CR>
nnoremap <leader>2 :call MakeFolds()<CR>
nnoremap <leader>3 :set spell!<CR>
nnoremap <leader>' :make %:t:r<CR>

nmap <silent>      [a :previous<CR>
nmap <silent>      ]a :next<CR>
nmap <silent>      [b :bprevious<CR>
nmap <silent>      ]b :bnext<CR>
nmap <silent>      [w <C-w>W
nmap <silent>      ]w <C-w>w
nmap <silent>      [t :tabprevious<CR>
nmap <silent>      ]t :tabnext<CR>
nmap <silent>      [e :cprevious<CR>
nmap <silent>      ]e :cnext<CR>
nmap <silent>      [l :lprevious<CR>
nmap <silent>      ]l :lnext<CR>

vnoremap <silent>* :<C-u>call setreg("/\V", escape(GetSelectedText(), '/\'))<CR>n
vnoremap <silent># :<C-u>call setreg("?\V", escape(GetSelectedText(), '/\'))<CR>n
vmap     v         <Plug>(expand_region_expand)
vmap     <C-v>     <Plug>(expand_region_shrink)

tnoremap <Esc>     <C-\><C-n>

" commands
command! -count MakeTags call MakeTags()
command! -count Open     !open %
command!        BufOnly  execute '%bdelete|edit #|normal `"'

" colors
hi ColorColumn                          ctermbg=8
hi Comment                                          ctermfg=7
hi DiffAdd                              ctermbg=8   ctermfg=10 cterm=none
hi DiffChange                           ctermbg=8   ctermfg=3  cterm=none
hi DiffDelete                           ctermbg=8   ctermfg=9  cterm=none
hi DiffText                             ctermbg=8   ctermfg=11 cterm=underline
hi Directory                                        ctermfg=4
hi FoldColumn                           ctermbg=0   ctermfg=7
hi Folded                               ctermbg=0   ctermfg=15 cterm=bold
hi GitSignsAdd                          ctermbg=0   ctermfg=10
hi GitSignsChange                       ctermbg=0   ctermfg=11
hi GitSignsCurrentLineBlame                         ctermfg=8
hi GitSignsDelete                       ctermbg=0   ctermfg=9
hi LspDiagnosticsSignError              ctermbg=0   ctermfg=1
hi LspDiagnosticsSignHint               ctermbg=0   ctermfg=7
hi LspDiagnosticsSignInfo               ctermbg=0   ctermfg=4
hi LspDiagnosticsSignWarning            ctermbg=0   ctermfg=16
hi LspDiagnosticsSignInformation        ctermbg=0   ctermfg=8
hi LspDiagnosticsVirtualTextError       ctermbg=0   ctermfg=1
hi LspDiagnosticsVirtualTextHint        ctermbg=0   ctermfg=7
hi LspDiagnosticsVirtualTextInfo        ctermbg=0   ctermfg=4
hi LspDiagnosticsVirtualTextWarning     ctermbg=0   ctermfg=16
hi LspDiagnosticsVirtualTextInformation ctermbg=0   ctermfg=8
hi LspDiagnosticsUnderlineError         ctermbg=0   ctermfg=1  cterm=underline
hi LspDiagnosticsUnderlineHint          ctermbg=0   ctermfg=7  cterm=underline
hi LspDiagnosticsUnderlineInfo          ctermbg=0   ctermfg=4  cterm=underline
hi LspDiagnosticsUnderlineWarning       ctermbg=0   ctermfg=16 cterm=underline
hi LspDiagnosticsUnderlineInformation   ctermbg=0   ctermfg=8  cterm=underline
hi LspReferenceRead                                 ctermfg=15 cterm=bold
hi LspReferenceWrite                                ctermfg=15 cterm=bold,underline
hi LspCodeLens                          ctermfg=22  ctermbg=0
hi NormalFloat                          ctermbg=0
hi FloatBorder                          ctermbg=0
hi Pmenu                                ctermbg=0   ctermfg=7
hi PmenuSbar                            ctermbg=0   ctermfg=7
hi PmenuSel                             ctermbg=4   ctermfg=0  cterm=bold
hi Search                                                      cterm=bold
hi SignColumn                           ctermbg=0
hi TabLine                              ctermbg=8   ctermfg=7  cterm=bold
hi TabLineFill                          ctermbg=0   ctermfg=0
hi TabLineSel                           ctermbg=7   ctermfg=0  cterm=bold
hi VertSplit                            ctermbg=8   ctermfg=8
hi Visual                               ctermbg=7   ctermfg=0
hi TelescopeSelection                   ctermbg=4   ctermfg=0  cterm=bold
hi TelescopeMatch                                              cterm=bold,underline

" autocommands
autocmd! BufWritePost *        :silent! MakeTags
autocmd! BufWritePre  *        :%s/\s\+$//e
autocmd! FileType     ansible  setlocal syntax=yaml

" undo sequence for space, dot and newline
inoremap <space> <C-G>u<space>
inoremap . <C-G>u.
inoremap <cr> <C-G>u<cr>
