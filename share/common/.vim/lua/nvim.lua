local buf_keymap     = vim.api.nvim_buf_set_keymap
local float_settings = { border = "rounded" }
local keymap         = vim.api.nvim_set_keymap
local opts           = { noremap = true, silent = true }
local servers        = { "hls" }
local kind_icons = {
  Text          = "",
  Method        = "m",
  Function      = "",
  Constructor   = "",
  Field         = "",
  Variable      = "",
  Class         = "",
  Interface     = "",
  Module        = "",
  Property      = "",
  Unit          = "",
  Value         = "",
  Enum          = "",
  Keyword       = "",
  Snippet       = "",
  Color         = "",
  File          = "",
  Reference     = "",
  Folder        = "",
  EnumMember    = "",
  Constant      = "",
  Struct        = "",
  Event         = "",
  Operator      = "",
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

----------------------------------------------------------------------- Comment

require("Comment").setup()

----------------------------------------------------------------------- lualine

require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "gruvbox-material",
    component_separators = { left = "", right = ""},
    section_separators = { left = "", right = ""},
    disabled_filetypes = { "coc-explorer" },
    always_divide_middle = true,
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = {
      "branch",
      "diff",
      "g:lsp_attached_server",
      { "diagnostics",
        sources = { "nvim_lsp" },
        sections = { "error", "warn", "info", "hint" },
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
      { "filename",
        file_status = true,
        path = 1,
        shorting_target = 10,
        symbols = {
          modified = " +",
          readonly = " -",
          unnamed = "¯\\_(ツ)_/¯",
        }
      }
    },
    lualine_x = {"encoding", { "fileformat", symbols = { unix = "unix", dos = "dos", mac = "mac" } } },
    lualine_y = {"filetype"},
    lualine_z = {"progress", "location"}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {"filename"},
    lualine_x = {"location"},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {"quickfix"}
}

---------------------------------------------------------------------- gitsigns

require("gitsigns").setup {
  signcolumn = true,
  current_line_blame = true,
  current_line_blame_opts = { delay = 500 },
  current_line_blame_formatter_opts = { relative_time = true },
  preview_config = float_settings,
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    keymap("n", "]h", "<cmd>lua require('gitsign').next_hunk()<cr>", {expr=true})
    keymap("n", "[h", "<cmd>lua require('gitsign').prev_hunk()<cr>", {expr=true})

    keymap("n", "<localleader>hs", "<cmd>lua require('gitsigns').stage_hunk()<cr>", opts)
    keymap("v", "<localleader>hs", "<cmd>lua require('gitsigns').stage_hunk()<cr>", opts)
    keymap("n", "<localleader>hr", "<cmd>lua require('gitsigns').reset_hunk()<cr>", opts)
    keymap("v", "<localleader>hr", "<cmd>lua require('gitsigns').reset_hunk()<cr>", opts)
    keymap("n", "<localleader>hS", "<cmd>lua require('gitsigns').stage_buffer()<cr>", opts)
    keymap("n", "<localleader>hu", "<cmd>lua require('gitsigns').undo_stage_hunk()<cr>", opts)
    keymap("n", "<localleader>hR", "<cmd>lua require('gitsigns').reset_buffer()<cr>", opts)
    keymap("n", "<localleader>hp", "<cmd>lua require('gitsigns').preview_hunk()<cr>", opts)

    keymap("o", "ih", ":<C-U>Gitsigns select_hunk<cr>", opts)
    keymap("x", "ih", ":<C-U>Gitsigns select_hunk<cr>", opts)
  end
}

----------------------------------------------------------------------- luasnip

local ls = require("luasnip")

ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
  ext_base_prio = 300,
  ext_prio_increase = 1,
  enable_autosnippets = true
}

require("luasnip/loaders/from_vscode").load()

keymap("i", "<c-h>", "<cmd>lua require('luasnip').change_choice(-1)<cr>", opts)
keymap("s", "<c-h>", "<cmd>lua require('luasnip').change_choice(-1)<cr>", opts)
keymap("i", "<c-l>", "<cmd>lua require('luasnip').change_choice(1)<cr>", opts)
keymap("s", "<c-l>", "<cmd>lua require('luasnip').change_choice(1)<cr>", opts)
keymap("i", "<c-j>", "<cmd>lua require('luasnip').jump(1)<cr>", opts)
keymap("s", "<c-j>", "<cmd>lua require('luasnip').jump(1)<cr>", opts)
keymap("i", "<c-k>", "<cmd>lua require('luasnip').jump(-1)<cr>", opts)
keymap("s", "<c-k>", "<cmd>lua require('luasnip').jump(-1)<cr>", opts)

--------------------------------------------------------------------------- cmp

local cmp = require("cmp")

cmp.setup({
  snippet = {
    expand = function(args)
      ls.lsp_expand(args.body)
    end,
  },
  mapping = {
    ["<C-d>"]     = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
    ["<C-f>"]     = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
    ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    ["<C-e>"]     = cmp.mapping { i = cmp.mapping.abort(), c = cmp.mapping.close() },
    ["<C-y>"]     = cmp.config.disable,
    ["<C-k>"]     = cmp.mapping.select_prev_item(),
    ["<C-j>"]     = cmp.mapping.select_next_item(),
    ["<CR>"]      = cmp.mapping.confirm { select = true },
    ["<Tab>"]     = cmp.mapping(function(fallback)
                    if cmp.visible() then cmp.select_next_item()
                    elseif ls.expandable() then  ls.expand()
                    elseif ls.expand_or_jumpable() then ls.expand_or_jump()
                    elseif check_backspace() then fallback()
                    else fallback()
                    end
                  end, { "i", "s"}),
    ["<S-Tab>"]   = cmp.mapping(function(fallback)
                    if cmp.visible() then cmp.select_prev_item()
                    elseif ls.jumpable(-1) then ls.jump(-1)
                    else fallback()
                    end
                  end, { "i", "s"}),
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      vim_item.kind = string.format("%s", kind_icons[vim_item.kind])
      vim_item.menu = ({
        buffer      = "[buf]",
        nvim_lsp    = "[lsp]",
        luasnip     = "[snip]",
        path        = "[path]",
        cmp_tabnine = "[tab9]",
      })[entry.source.name]
      return vim_item
    end,
  },
  documentation = { border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" }, },
  sources = cmp.config.sources({
    { name = "luasnip" },
    { name = "nvim_lsp" },
    { name = "cmp_tabnine" },
    { name = "buffer" }
  }, {
    { name = "path" }
  })
})

cmp.setup.cmdline("?", {
  sources = cmp.config.sources({ { name = "buffer" } }, {})
})

cmp.setup.cmdline("/", {
  sources = cmp.config.sources({ { name = "buffer" } }, {})
})

cmp.setup.cmdline(":", {
  sources = cmp.config.sources({ { name = "path" } }, { { name = "cmdline" } })
})

--------------------------------------------------------------------- lspconfig

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, float_settings)

for _, lsp in pairs(servers) do
  require("lspconfig")[lsp].setup {
    capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
    autostart = true,
    flags = { debounce_text_changes = 150 },
    on_attach = function(client, bufnr)
      vim.g.lsp_attached_server = lsp

      buf_keymap(bufnr, "n", "<localleader>,", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)
      buf_keymap(bufnr, "n", "<localleader>.", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
      buf_keymap(bufnr, "n", "<localleader>R", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
      buf_keymap(bufnr, "n", "<localleader>d", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>", opts)
      buf_keymap(bufnr, "n", "<localleader>m", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
      buf_keymap(bufnr, "n", "<localleader>r", "<cmd>lua require('telescope.builtin').lsp_references()<cr>", opts)
      keymap(           "n", "<localleader>D", "<cmd>lua require('telescope.builtin').lsp_document_diagnostics()<cr>", opts)
      keymap(           "n", "<localleader>a", "<cmd>lua require('telescope.builtin').lsp_code_actions()<cr>", opts)
      keymap(           "n", "<localleader>f", "<cmd>lua vim.lsp.buf.formatting()<cr>", opts)
      keymap(           "n", "<localleader>l", "<cmd>lua vim.diagnostic.setloclist({open_loclist = false})<cr>", opts)
      keymap(           "n", "<localleader>s", "<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<cr>", opts)
      buf_keymap(bufnr, "n", "gD",             "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
      buf_keymap(bufnr, "n", "gd",             "<cmd>lua vim.lsp.buf.definition()<cr>", opts)
      buf_keymap(bufnr, "n", "gi",             "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
      keymap(           "n", "[d",             "<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>", opts)
      keymap(           "n", "]d",             "<cmd>lua vim.lsp.diagnostic.goto_next()<cr>", opts)

      vim.api.nvim_command [[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]]
      vim.api.nvim_command [[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]]
      vim.api.nvim_command [[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]]
    end
  }
end

--------------------------------------------------------------------- which-key

local wk = require("which-key")

wk.register({
  ["<leader>\\"]      = { "<cmd>lua require('telescope.builtin').git_files({hidden=true})<cr>",                   "Search git files only" },
  ["<leader><esc>"]   = { "<cmd>lua require('telescope.builtin').resume()<cr>",                                   "Last search" },
  ["<leader>/"]       = { "<cmd>lua require('telescope.builtin').file_browser({hidden=true,no_ignore=true})<cr>", "Browse files" },
  ["<leader>b"]       = { "<cmd>lua require('telescope.builtin').buffers()<cr>",                                  "Search buffers" },
  ["<leader>c"]       = { "<cmd>split term://fish<cr>",                                                           "Open terminal" },
  ["<leader>f"]       = { "<cmd>lua require('telescope.builtin').live_grep()<cr>",                                "Grep files" },
  ["<leader>F"]       = { "<cmd>lua require('telescope.builtin').find_files()<cr>",                               "Search files" },
  ["<leader>h"]       = { "<cmd>lua require('telescope.builtin').jumplist()<cr>",                                 "Search history" },
  ["<leader>m"]       = { "<cmd>lua require('telescope.builtin').marks()<cr>",                                    "Search marks" },
  ["<leader>r"]       = { "<cmd>lua require('telescope.builtin').registers()<cr>",                                "Search registers" },
  ["<leader>t"]       = { "<cmd>lua require('telescope.builtin').tags()<cr>",                                     "Search tags" },
  ["<leader>g"]       = { name = "Git" },
  ["<leader>gb"]      = { "<cmd>lua require('telescope.builtin').git_branches()<cr>",                             "List branches" },
  ["<leader>gB"]      = { ":Git blame<cr>",                                                                       "Blame" },
  ["<leader>gd"]      = { "<cmd>lua require('gitsigns').diffthis()<cr>",                                          "Diff this" },
  ["<leader>gc"]      = { ":Git commit<cr>",                                                                      "Commit" },
  ["<leader>gg"]      = { ":G<cr><C-w>10_",                                                                       "Fugitive status" },
  ["<leader>gl"]      = { "<cmd>lua require('telescope.builtin').git_commits()<cr>",                              "List commits" },
  ["<leader>gL"]      = { "<cmd>lua require('telescope.builtin').git_bcommits()<cr>",                             "List commits for buffer" },
  ["<leader>gp"]      = { ":G pull<cr>",                                                                          "Pull" },
  ["<leader>gP"]      = { ":G push<cr>",                                                                          "Push" },
  ["<leader>gq"]      = { "<cmd>lua require('gitsigns').setqflist()<cr>",                                         "Quickfix list" },
  ["<leader>gQ"]      = { "<cmd>lua require('gitsigns').setloclist()<cr>",                                        "Local list" },
  ["<leader>gs"]      = { "<cmd>lua require('telescope.builtin').git_status()<cr>",                               "Show status" },
  ["<leader>gS"]      = { "<cmd>lua require('telescope.builtin').git_stash()<cr>",                                "Show stash" },
  ["<leader><space>"] = { "<cmd>lua require('telescope.builtin').builtin()<cr>",                                  "Other search options" },
  ["<leader>l"]       = { name = "LSP" },
  ["<leader>lh"]      = { "<cmd>LspStart hls<cr>",                                                                "LSP start hls" },
  ["<leader>li"]      = { "<cmd>LspInfo<cr>",                                                                     "LSP info" },
  ["<leader>z"]       = { "<cmd>TZAtaraxis<cr>",                                                                  "ZEN" },

  ["<localleader>h"]  = { name = "Git hunk" },
  ["<localleader>hb"] = { "<cmd>lua require('gitsigns').blame_line({full = true})<cr>",                           "Blame line" },
  ["<localleader>hp"] = { "<cmd>lua require('gitsigns').preview_hunk()<cr>",                                      "Preview hunk" },
  ["<localleader>hr"] = { "<cmd>lua require('gitsigns').reset_hunk()<cr>",                                        "Reset hunk" },
  ["<localleader>hR"] = { "<cmd>lua require('gitsigns').reset_buffer()<cr>",                                      "Reset buffer" },
  ["<localleader>hs"] = { "<cmd>lua require('gitsigns').stage_hunk()<cr>",                                        "Stage hunk" },
  ["<localleader>hS"] = { "<cmd>lua require('gitsigns').stage_buffer()<cr>",                                      "Stage buffer" },
  ["<localleader>hu"] = { "<cmd>lua require('gitsigns').undo_stage_hunk()<cr>",                                   "Undo stage hunk" },
  ["<localleader>,"]  = { "<cmd>lua vim.lsp.buf.hover()<cr>",                                                     "Show documentation" },
  ["<localleader>."]  = { "<cmd>lua vim.lsp.buf.type_definition()<cr>",                                           "Type definition" },
  ["<localleader>a"]  = { "<cmd>lua require('telescope.builtin').lsp_code_actions()<cr>",                         "Code actions" },
  ["<localleader>d"]  = { "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>",                              "Show diagnostics" },
  ["<localleader>D"]  = { "<cmd>lua require('telescope.builtin').lsp_document_diagnostics()<cr>",                 "List diagnostics" },
  ["<localleader>f"]  = { "<cmd>lua vim.lsp.buf.formatting()<cr>",                                                "Format" },
  ["<localleader>l"]  = { "<cmd>lua vim.diagnostic.setloclist({open_loclist = false})<cr>",                       "Load diagnostics to loclist" },
  ["<localleader>m"]  = { "<cmd>lua vim.lsp.buf.signature_help()<cr>",                                            "Signature help" },
  ["<localleader>r"]  = { "<cmd>lua require('telescope.builtin').lsp_references()<cr>",                           "List references" },
  ["<localleader>R"]  = { "<cmd>lua vim.lsp.buf.rename()<cr>",                                                    "Rename" },
  ["<localleader>s"]  = { "<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<cr>",                    "List workspace symbols" }
}, { mode = "n" })

wk.register({
  ["<localleader>t"]      = { name = "Tabularize" },
  ["<localleader>t="]     = { ":Tab /^[^=]*\\zs=/l1c1l0<cr>",                                                     "Align to '=' symbol" },
  ["<localleader>t<bar>"] = { ":Tab /|<cr>",                                                                      "Align markdown table" },
  ["<localleader>t:"]     = { ":Tab /^[^:]*\\zs:/l1c0l0<cr>",                                                     "Align to first symbol" },
  ["<localleader>t;"]     = { ":Tab /^[^:]*:\zs/l1l0",                                                            "Align to key in hash" },
  ["<localleader>tt"]     = { ":Tabularize /",                                                                    "Custom alignment", silent = false }
}, { mode = "v" })
