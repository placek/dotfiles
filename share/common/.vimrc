source ~/.vim/init/general.vim

if $VIM_IDE == "yes"
  source ~/.vim/init/coc.vim
  source ~/.vim/init/fzf.vim
  source ~/.vim/init/gitgutter.vim
  source ~/.vim/init/expand_region.vim
  source ~/.vim/init/tabular.vim
else
  nnoremap <localleader>lc :source ~/.vim/init/coc.vim<CR>
  nnoremap <localleader>lf :source ~/.vim/init/fzf.vim<CR>
  nnoremap <localleader>lg :source ~/.vim/init/gitgutter.vim<CR>
  nnoremap <localleader>le :source ~/.vim/init/expand_region.vim<CR>
  nnoremap <localleader>lt :source ~/.vim/init/tabular.vim<CR>
endif
