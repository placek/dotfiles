local ls     = require("luasnip")
local cmp    = require("cmp")
local keymap = vim.api.nvim_set_keymap
local opts   = { noremap = true, silent = true }

ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
  ext_base_prio = 300,
  ext_prio_increase = 1,
  enable_autosnippets = true
}

require("luasnip/loaders/from_vscode").load()

keymap("i", "<m-h>", "<cmd>lua require('luasnip').change_choice(-1)<cr>", opts)
keymap("s", "<m-h>", "<cmd>lua require('luasnip').change_choice(-1)<cr>", opts)
keymap("i", "<m-l>", "<cmd>lua require('luasnip').change_choice(1)<cr>", opts)
keymap("s", "<m-l>", "<cmd>lua require('luasnip').change_choice(1)<cr>", opts)
keymap("i", "<m-j>", "<cmd>lua require('luasnip').jump(1)<cr>", opts)
keymap("s", "<m-j>", "<cmd>lua require('luasnip').jump(1)<cr>", opts)
keymap("i", "<m-k>", "<cmd>lua require('luasnip').jump(-1)<cr>", opts)
keymap("s", "<m-k>", "<cmd>lua require('luasnip').jump(-1)<cr>", opts)

cmp.setup {
  snippet = {
    expand = function(args)
      ls.lsp_expand(args.body)
    end,
  },
  mapping = {
    ["<c-d>"]     = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
    ["<c-u>"]     = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
    ["<left>"]    = cmp.mapping { i = cmp.mapping.abort(), c = cmp.mapping.close() },
    ["<right>"]   = cmp.mapping.confirm { select = true, },
    ["<down>"]    = cmp.mapping(function(fallback)
                    if cmp.visible() then cmp.select_next_item()
                    elseif ls.expandable() then ls.expand()
                    else fallback()
                    end
                  end, { "i", "s"}),
    ["<up>"]      = cmp.mapping(function(fallback)
                    if cmp.visible() then cmp.select_prev_item()
                    else fallback()
                    end
                  end, { "i", "s"}),
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      vim_item.kind = ({
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
      })[vim_item.kind]
      vim_item.menu = ({
        buffer      = "",
        nvim_lsp    = "",
        luasnip     = "",
        path        = "",
        cmp_tabnine = "9",
      })[entry.source.name]
      return vim_item
    end,
  },
  window = {
    documentation = cmp.config.window.bordered(),
  },
  sources = cmp.config.sources({
    { name = "luasnip" },
    { name = "nvim_lsp" },
    { name = "cmp_tabnine" },
    { name = "buffer" },
  }, {
    { name = "path" },
  })
}

cmp.setup.cmdline("?", {
  sources = cmp.config.sources({ { name = "buffer", } }, {}),
})

cmp.setup.cmdline("/", {
  sources = cmp.config.sources({ { name = "buffer", } }, {}),
})

cmp.setup.cmdline(":", {
  sources = cmp.config.sources({ { name = "path", } }, { { name = "cmdline", } }),
})
