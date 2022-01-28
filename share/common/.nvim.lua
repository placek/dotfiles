-- TODO:
-- * git gutter
-- * expand region

vim.opt.backup         = false
vim.opt.clipboard      = "unnamedplus"
vim.opt.cmdheight      = 2
vim.opt.colorcolumn    = { "80", "160" }
vim.opt.completeopt    = { "menuone", "noselect" }
vim.opt.conceallevel   = 0
vim.opt.cursorline     = true
vim.opt.expandtab      = true
vim.opt.fileencoding   = "utf-8"
vim.opt.foldcolumn     = "1"
vim.opt.foldmethod     = "manual"
vim.opt.grepformat     = "%f:%l:%c:%m"
vim.opt.grepprg        = "rg --vimgrep $*"
vim.opt.hlsearch       = true
vim.opt.list           = true
vim.opt.listchars      = "tab:» ,nbsp:␣,trail:·,extends:›,precedes:‹"
vim.opt.mouse          = "a"
vim.opt.number         = true
vim.opt.numberwidth    = 2
vim.opt.pumheight      = 10
vim.opt.relativenumber = true
vim.opt.scrolloff      = 8
vim.opt.shiftwidth     = 2
vim.opt.showmode       = false
vim.opt.sidescrolloff  = 8
vim.opt.signcolumn     = "yes"
vim.opt.smartindent    = true
vim.opt.splitbelow     = true
vim.opt.splitright     = true
vim.opt.swapfile       = false
vim.opt.tabstop        = 2
vim.opt.timeoutlen     = 1000
vim.opt.undofile       = true
vim.opt.updatetime     = 300
vim.opt.wrap           = false
vim.opt.writebackup    = false

vim.opt.shortmess:append "c"

local opts = { noremap = true, silent = true }
local term_opts = { silent = true }
local keymap = vim.api.nvim_set_keymap
local buf_keymap = vim.api.nvim_buf_set_keymap
local kind_icons = {
  Text = "",
  Method = "m",
  Function = "",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "",
  Interface = "",
  Module = "",
  Property = "",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = "",
}
local signs = {
  { name = "LspDiagnosticsSignError",       text = "" },
  { name = "LspDiagnosticsSignWarning",     text = "" },
  { name = "LspDiagnosticsSignHint",        text = "" },
  { name = "LspDiagnosticsSignInformation", text = "" },
}

for _, sign in ipairs(signs) do
  vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

vim.g.mapleader = "\\"
vim.g.maplocalleader = ","

keymap("n", "<leader>1",       "<cmd>set number!<CR>", opts)
keymap("n", "<leader>2",       "<cmd>set relativenumber!<CR>", opts)
keymap("n", "<leader>3",       "<cmd>set hlsearch!<CR>", opts)
keymap("n", "<leader>b",       "<cmd>lua require('telescope.builtin').buffers()<cr>", opts)
keymap("n", "<leader>f",       "<cmd>lua require('telescope.builtin').live_grep()<cr>", opts)
keymap("n", "<leader>F",       "<cmd>lua require('telescope.builtin').find_files()<cr>", opts)
keymap("n", "<leader>gS",      "<cmd>lua require('telescope.builtin').git_stash()<cr>", opts)
keymap("n", "<leader>gc",      "<cmd>lua require('telescope.builtin').git_commits()<cr>", opts)
keymap("n", "<leader>gf",      "<cmd>lua require('telescope.builtin').git_files()<cr>", opts)
keymap("n", "<leader>gs",      "<cmd>lua require('telescope.builtin').git_status()<cr>", opts)
keymap("n", "<leader>h",       "<cmd>lua require('telescope.builtin').jumplist()<cr>", opts)
keymap("n", "<leader>m",       "<cmd>lua require('telescope.builtin').marks()<cr>", opts)
keymap("n", "<leader>r",       "<cmd>lua require('telescope.builtin').registers()<cr>", opts)
keymap("n", "<leader>t",       "<cmd>lua require('telescope.builtin').tags()<cr>", opts)
keymap("n", "<leader>\\",      "<cmd>lua require('telescope.builtin').file_browser()<cr>", opts)
keymap("n", "<leader><space>", "<cmd>lua require('telescope.builtin').builtin()<cr>", opts)
keymap("v", "v",               "<Plug>(expand_region_expand)", opts)
keymap("v", "<C-v>",           "<Plug>(expand_region_shrink)", opts)


require('Comment').setup()

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
        sources = { 'nvim_lsp' },
        sections = { 'error', 'warn', 'info', 'hint' },
        symbols = {
          error = " ",
          warn  = " ",
          info  = " ",
          hint  = " "
        },
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
  extensions = {'quickfix'}
}

require('cmp').setup {
  sources = {
    { name = 'nvim_lsp' },
    { name = 'cmp_tabnine' }
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      vim_item.kind = string.format("%s", kind_icons[vim_item.kind])
      vim_item.menu = ({
        nvim_lsp = "[LSP]",
        cmp_tabnine = "[TAB9]"
      })[entry.source.name]
      return vim_item
    end,
  },
  experimental = {
    ghost_text = false,
    native_menu = false
  }
}

local opts = { noremap=true, silent=true }

local on_attach = function(client, bufnr)
  buf_keymap(bufnr, 'n', '<localleader>,', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
  keymap(           'n', '<localleader>a', '<cmd>lua require("telescope.builtin").lsp_code_actions()<cr>', opts)
  buf_keymap(bufnr, 'n', '<localleader>d', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>', opts)
  keymap(           'n', '<localleader>D', '<cmd>lua require("telescope.builtin").lsp_document_diagnostics()<cr>', opts)
  keymap(           'n', '<localleader>f', '<cmd>lua vim.lsp.buf.formatting()<cr>', opts)
  buf_keymap(bufnr, 'n', '<localleader>r', '<cmd>lua require("telescope.builtin").lsp_references()<cr>', opts)
  buf_keymap(bufnr, 'n', '<localleader>R', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
  keymap(           'n', '<localleader>s', '<cmd>lua require("telescope.builtin").lsp_workspace_symbols()<cr>', opts)
  buf_keymap(bufnr, 'n', 'gd',             '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
  keymap(           'n', '[d',             '<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>', opts)
  keymap(           'n', ']d',             '<cmd>lua vim.lsp.diagnostic.goto_next()<cr>', opts)

  vim.api.nvim_command [[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]]
  vim.api.nvim_command [[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]]
  vim.api.nvim_command [[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]]
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local servers = { 'hls' }

for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    capabilities = capabilities,
    autostart = true,
    on_attach = on_attach,
    flags = { debounce_text_changes = 150 }
  }
end

vim.cmd [[
" commands
command! -count MakeTags !git tags
command! -count Open     !open %

" colors
hi ColorColumn                      ctermbg=18
hi DiffAdd                          ctermbg=2  ctermfg=0 cterm=BOLD
hi DiffChange                       ctermbg=3  ctermfg=0 cterm=BOLD
hi DiffDelete                       ctermbg=1  ctermfg=0 cterm=BOLD
hi DiffText                         ctermbg=2  ctermfg=0 cterm=BOLD
hi Directory                        ctermfg=4
hi FoldColumn                       ctermbg=0  ctermfg=7
hi Folded                           ctermbg=6  ctermfg=0
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
hi Pmenu                            ctermbg=18 ctermfg=7
hi PmenuSbar                        ctermbg=0  ctermfg=8
hi Search                           ctermbg=2  ctermfg=0
hi SignColumn                       ctermbg=0
hi TabLine                          ctermbg=0  ctermfg=7 cterm=NONE
hi TabLineFill                      ctermbg=0  ctermfg=0
hi TabLineSel                       ctermbg=0  ctermfg=9 cterm=NONE
hi VertSplit                        ctermbg=8  ctermfg=8
hi Visual                           ctermbg=7  ctermfg=0
hi gitblame                         ctermfg=8

" autocommands
autocmd! BufWritePost * :silent! MakeTags
autocmd! BufWritePre * :%s/\s\+$//e
]]
