source ~/.vim/init/general.vim

if $VIM_IDE == "yes"
  source ~/.vim/init/ale.vim
  source ~/.vim/init/fzf.vim
  source ~/.vim/init/gitgutter.vim
  source ~/.vim/init/tabular.vim
else
  nnoremap <localleader>a :source ~/.vim/init/ale.vim<CR>
  nnoremap <localleader>f :source ~/.vim/init/fzf.vim<CR>
  nnoremap <localleader>g :source ~/.vim/init/gitgutter.vim<CR>
  nnoremap <localleader>t :source ~/.vim/init/tabular.vim<CR>
endif
