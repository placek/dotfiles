" functions
function! s:getSelectedText()
  norm gv"sy
  let l:ret = getreg('s')
  exe "norm \<Esc>"
  return l:ret
endfunction

function! s:placeComment()
  let l:comment_pattern = substitute(escape(&commentstring, '^$.*?/\[]'), '%s', "\\&", '')
  execute 's/\%V.*/'.l:comment_pattern
endfunction

function! s:makeTags()
  let tags_job = job_start("git ctags", #{ exit_cb: function('MakeTagsResult') })
endfunction

function! s:searchWithVimgrep(query, files = "**/*")
  if len(a:query) != 0
    execute "lvimgrep /".escape(a:query, '^$.*?/\[]')."/g ".a:files
  endif
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

function! MakeTagsResult(job, status)
  if a:status == 0
    echom "MakeTags: done"
  else
    echom "MakeTags: tags generation failed"
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

" settings
set backspace=indent,eol,start
set clipboard=unnamedplus
set cmdheight=2
set colorcolumn=80,160
set completeopt=longest,menuone,noselect
set cursorline
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
set diffopt-=internal
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
set nobackup
set noshowmode
set nospell
set nowritebackup
set number
set omnifunc=syntaxcomplete#Complete
set relativenumber
set shiftwidth=2
set shortmess=a
set showcmd
set signcolumn=yes
set softtabstop=2
set swapfile
set tabstop=2
set termencoding=utf-8
set timeoutlen=1000 ttimeoutlen=0
set ttyfast
set updatetime=300
set wildignore=.*,.git/

" status
if !has('nvim')
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
else
  lua << END
    require('lualine').setup {
      options = {
        icons_enabled = true,
        theme = 'gruvbox-material',
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
        disabled_filetypes = { 'coc-explorer' },
        always_divide_middle = true,
      },
      sections = {
        lualine_a = {'mode'},
        lualine_b = {
          'branch',
          'diff',
          { 'diagnostics',
            sources = { 'coc' },
            sections = { 'error', 'warn', 'info', 'hint' },
            symbols = { error = 'E', warn = 'W', info = 'I', hint = 'H' },
            update_in_insert = false,
            always_visible = false,
            colored = true,
          }
        },
        lualine_c = {
          { 'filename',
            file_status = true,
            path = 1,
            shorting_target = 10,
            symbols = {
              modified = ' [+]',
              readonly = ' [-]',
              unnamed = '[No Name]',
            }
          }
        },
        lualine_x = {'encoding', { 'fileformat', symbols = { unix = 'unix', dos = 'dos', mac = 'mac' } } },
        lualine_y = {'filetype'},
        lualine_z = {'progress', 'location'}
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {}
      },
      tabline = {},
      extensions = {'quickfix', 'fzf'}
    }
END
endif

" options
let g:mapleader = "\\"
let g:maplocalleader = ","

" mappings
nnoremap <leader>1  :set number!<CR>
nnoremap <leader>2  :set relativenumber!<CR>
nnoremap <leader>3  :set hlsearch!<CR>

nnoremap <leader>b  :ls<CR>
nnoremap <leader>f  :call <SID>searchWithVimgrep(input("/"))<CR>
nnoremap <leader>F  :find
nnoremap <leader>gc :!git l<CR>
nnoremap <leader>gf :!git ls-files<CR>
nnoremap <leader>gs :!git st<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>h  :jumps<CR>
nnoremap <leader>m  :marks<CR>
nnoremap <leader>s  :set cursorbind!<CR>

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

nnoremap <silent>g/ :call <SID>searchWithVimgrep(expand('<cword>'))<CR>
vnoremap <silent>*  :<C-u>call setreg("/", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent>#  :<C-u>call setreg("?", substitute(<SID>getSelectedText(), '\_s\+', '\\_s\\+', 'g'))<CR>n
vnoremap <silent>g/ :<C-u>call <SID>searchWithVimgrep(<SID>getSelectedText())<CR>
vmap     v          <Plug>(expand_region_expand)
vmap     <C-v>      <Plug>(expand_region_shrink)

if !has('nvim')
  nnoremap <leader>c  :terminal ++close<CR>
  vnoremap <silent>gc :call <SID>placeComment()<CR>
else
  nnoremap <leader>c  :split term://fish<CR>
  tnoremap <Esc> <C-\><C-n>
  lua require('Comment').setup()
endif

" commands
command! -count MakeTags     call <SID>makeTags()
command! -count Open         !open %

" colors
hi clear StatusLineNC
hi gitblame       ctermfg=8
hi ColorColumn    ctermbg=18
hi DiffAdd        ctermbg=2 ctermfg=0 cterm=BOLD
hi DiffChange     ctermbg=3 ctermfg=0 cterm=BOLD
hi DiffDelete     ctermbg=1 ctermfg=0 cterm=BOLD
hi DiffText       ctermbg=2 ctermfg=0 cterm=BOLD
hi Directory      ctermfg=4
hi FoldColumn     ctermbg=0 ctermfg=7
hi Folded         ctermbg=6 ctermfg=0
hi Pmenu          ctermbg=8
hi Search         ctermbg=2 ctermfg=0
hi SignColumn     ctermbg=0
hi StatusLine     ctermbg=0 ctermfg=7 cterm=NONE
hi StatusLineInfo ctermbg=0 ctermfg=8 cterm=BOLD
hi StatusLineMode ctermbg=16 ctermfg=0
hi TabLine        ctermbg=0 ctermfg=7 cterm=NONE
hi TabLineFill    ctermbg=0 ctermfg=0
hi TabLineSel     ctermbg=0 ctermfg=9 cterm=NONE
hi VertSplit      ctermbg=8 ctermfg=8
hi Visual         ctermbg=7 ctermfg=0
hi MatchParen     ctermbg=18 ctermfg=2

" autocommands
autocmd! BufWritePost * :silent! MakeTags
autocmd! BufWritePre * :%s/\s\+$//e
autocmd! FileType make setlocal noexpandtab
