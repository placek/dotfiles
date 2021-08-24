packadd ale

" settings
set omnifunc=ale#completion#OmniFunc

" options
let g:ale_completion_autoimport = 1
let g:ale_completion_enabled = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_format = '%severity%: %s'
let g:ale_echo_msg_info_str = 'I'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_hover_cursor=1
let g:ale_loclist_msg_format = '%s'
let g:ale_sign_error = 'E'
let g:ale_sign_info = 'I'
let g:ale_sign_style_error = 'S'
let g:ale_sign_style_warning = 's'
let g:ale_sign_warning = 'W'

let g:ale_haskell_hls_executable = 'haskell-language-server'

" mappings
nnoremap <silent>K  :ALEHover<CR>
nnoremap <silent>gd :ALEGoToDefinition<CR>

" colors
hi ALEError ctermbg=8
hi ALEErrorSign ctermbg=0 ctermfg=1 cterm=BOLD
hi ALEStyleError ctermbg=8
hi ALEStyleErrorSign ctermbg=0 ctermfg=1
hi ALEStyleWarning ctermbg=8
hi ALEStyleWarningSign ctermbg=0 ctermfg=9
hi ALEWarning ctermbg=8
hi ALEWarningSign ctermbg=0 ctermfg=9 cterm=BOLD
