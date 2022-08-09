require("gitsigns").setup {
  signcolumn = true,
  current_line_blame = true,
  current_line_blame_opts = { delay = 500 },
  current_line_blame_formatter_opts = { relative_time = true },
  preview_config = float_settings,
  on_attach = function(bufnr)
    local keymap = vim.api.nvim_set_keymap
    local opts = { noremap = true, silent = true }

    keymap("n", "]h", "<cmd>lua require('gitsigns').next_hunk()<cr>", opts)
    keymap("n", "[h", "<cmd>lua require('gitsigns').prev_hunk()<cr>", opts)
    keymap("o", "ih", ":<C-U>Gitsigns select_hunk<cr>", opts)
    keymap("x", "ih", ":<C-U>Gitsigns select_hunk<cr>", opts)
  end
}
