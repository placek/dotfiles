packadd ale

" settings
set omnifunc=ale#completion#OmniFunc

" options
let g:ale_completion_autoimport = 1
let g:ale_completion_enabled    = 1
let g:ale_echo_msg_error_str    = 'E'
let g:ale_echo_msg_format       = '%severity%: %s'
let g:ale_echo_msg_info_str     = 'I'
let g:ale_echo_msg_warning_str  = 'W'
let g:ale_hover_cursor          = 1
let g:ale_loclist_msg_format    = '%s'
let g:ale_set_loclist           = 0
let g:ale_set_quickfix          = 1
let g:ale_sign_error            = 'E'
let g:ale_sign_info             = 'I'
let g:ale_sign_style_error      = 'S'
let g:ale_sign_style_warning    = 's'
let g:ale_sign_warning          = 'W'

" mappings
nnoremap <silent>K  :ALEHover<CR>
nnoremap <silent>gd :ALEGoToDefinition<CR>

nnoremap <localleader>f :ALEFix<CR>
nnoremap <localleader>c :ALECodeAction<CR>
nnoremap <localleader>d :ALEDetail<CR>
nnoremap <localleader>r :ALERename<CR>

" colors
hi ALEError            ctermbg=8
hi ALEErrorSign        ctermbg=0 ctermfg=1 cterm=BOLD
hi ALEStyleError       ctermbg=8
hi ALEStyleErrorSign   ctermbg=0 ctermfg=1
hi ALEStyleWarning     ctermbg=8
hi ALEStyleWarningSign ctermbg=0 ctermfg=9
hi ALEWarning          ctermbg=8
hi ALEWarningSign      ctermbg=0 ctermfg=9 cterm=BOLD

" hack for haskell:
" creates a separate cabal linter that does not cd into a file dir and uses
" cwd as a project directory
call ale#linter#Define('haskell', {
\  'name':          'my_cabal',
\  'aliases':       ['my-cabal'],
\  'output_stream': 'stderr',
\  'executable':    'cabal',
\  'command':       '%e build -fno-code -v0 -- %s </dev/null',
\  'callback':      'ale#handlers#haskell#HandleGHCFormat',
\})

if !exists('g:ale_linters')
  let g:ale_linters = {}
endif
let g:ale_linters.haskell = ['my-cabal', 'hls', 'hlint']
let g:ale_fixers.haskell = 'stylish-haskell'
let g:ale_haskell_hls_executable = 'haskell-language-server'
