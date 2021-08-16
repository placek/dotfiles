" options
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_format = '%severity%: %s'
let g:ale_echo_msg_info_str = 'I'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_haskell_hls_executable = 'haskell-language-server'
let g:ale_hover_cursor=1
let g:ale_loclist_msg_format = '%s'
let g:ale_sign_error = 'E'
let g:ale_sign_info = 'I'
let g:ale_sign_style_error = 'S'
let g:ale_sign_style_warning = 's'
let g:ale_sign_warning = 'W'

" colors
hi ALEErrorSign ctermbg=0 ctermfg=1 cterm=BOLD
hi ALEWarningSign ctermbg=0 ctermfg=9 cterm=BOLD
hi ALEStyleErrorSign ctermbg=0 ctermfg=1
hi ALEStyleWarningSign ctermbg=0 ctermfg=9
hi ALEError ctermbg=8
hi ALEWarning ctermbg=8
hi ALEStyleError ctermbg=8
hi ALEStyleWarning ctermbg=8
