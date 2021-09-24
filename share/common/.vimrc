source ~/.vim/init/general.vim

if $VIM_IDE == "yes"
  source ~/.vim/init/ale.vim
  source ~/.vim/init/fzf.vim
  source ~/.vim/init/gitgutter.vim
  source ~/.vim/init/markdown.vim
  source ~/.vim/init/tabular.vim
else
  nnoremap <localleader>la :source ~/.vim/init/ale.vim<CR>
  nnoremap <localleader>lf :source ~/.vim/init/fzf.vim<CR>
  nnoremap <localleader>lg :source ~/.vim/init/gitgutter.vim<CR>
  nnoremap <localleader>lm :source ~/.vim/init/markdown.vim<CR>
  nnoremap <localleader>lt :source ~/.vim/init/tabular.vim<CR>
endif
