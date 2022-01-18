set runtimepath^=~/.vim
let &packpath = &runtimepath
source ~/.vimrc

packadd lualine-nvim
lua require('lualine').setup({ options = { theme = 'gruvbox-material' } })
