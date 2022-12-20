local lsp   = require('lspconfig')
local flags = { allow_incremental_sync = true, debounce_text_changes = 200, }
local signs = {
  { name = "LspDiagnosticsSignError",       text = "" },
  { name = "LspDiagnosticsSignWarning",     text = "" },
  { name = "LspDiagnosticsSignHint",        text = "" },
  { name = "LspDiagnosticsSignInformation", text = "" },
}

-- LSP settings (for overriding per client)
local handlers =  {
  ["textDocument/hover"]         = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
  ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
}

for _, sign in ipairs(signs) do
  vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

local function on_attach(client, buf)
  local buf_keymap = vim.api.nvim_buf_set_keymap
  local keymap     = vim.api.nvim_set_keymap
  local opts       = { noremap = true, silent = true }

  vim.lsp.codelens.refresh()
  vim.diagnostic.config({ virtual_text = false })

  buf_keymap(buf, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", opts)
  keymap(         "n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>", opts)
  keymap(         "n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<cr>", opts)

  vim.api.nvim_command [[ autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight() ]]
  vim.api.nvim_command [[ autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight() ]]
  vim.api.nvim_command [[ autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references() ]]
  vim.api.nvim_command [[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, { border = "rounded", focus = false })]]
end

-- haskell
lsp.hls.setup {
  autostart    = true,
  flags        = flags,
  on_attach    = on_attach,
  handlers     = handlers,
  cmd          = { "haskell-language-server", "--lsp" },
  settings     = {
    haskell = {
      formattingProvider = "stylish-haskell",
      plugin = {
        alternateNumberFormat = { globalOn = true, },
        callHierarchy         = { globalOn = true, },
        class                 = { globalOn = true, },
        eval                  = { globalOn = true, },
        haddockComments       = { globalOn = true, },
        hlint                 = { globalOn = true, },
        importLens            = { globalOn = true, },
        moduleName            = { globalOn = true, },
        pragmas               = { globalOn = true, },
        qualifyImportedNames  = { globalOn = true, },
        refineImports         = { globalOn = true, },
        retrie                = { globalOn = true, },
        selectionRange        = { globalOn = true, },
        splice                = { globalOn = true, },
        tactics               = { globalOn = true, },
        -- "ghcide-code-actions-bindings" = { globalOn = true, },
        -- "ghcide-code-actions-fill-holes" = { globalOn = true, },
        -- "ghcide-code-actions-imports-exports" = { globalOn = true, },
        -- "ghcide-code-actions-type-signatures" = { globalOn = true, },
        -- "ghcide-completions" = { globalOn = true, },
        -- "ghcide-core" = { globalOn = true, },
        -- "ghcide-hover-and-symbols" = { globalOn = true, },
        -- "ghcide-type-lenses" = { globalOn = true, },
      },
    }
  },
}
