local wk = require("which-key")
local cth = require('telescope.themes').get_cursor()

wk.register({
  ["<leader><esc>"]   = { "<cmd>lua require('telescope.builtin').resume()<cr>",                   "Last search" },
  ["<leader><space>"] = { "<cmd>lua require('telescope.builtin').builtin()<cr>",                  "Other search options" },
  ["<leader>\\"]      = { "<cmd>lua require('telescope.builtin').git_files({hidden=true})<cr>",   "Search git files only" },
  ["<leader>/"]       = { "<cmd>Telescope file_browser path=%:p:h hidden=true<cr>",               "Browse files" },
  ["<leader>b"]       = { "<cmd>lua require('telescope.builtin').buffers()<cr>",                  "Search buffers" },
  ["<leader>F"]       = { "<cmd>lua require('telescope.builtin').find_files()<cr>",               "Search files" },
  ["<leader>f"]       = { "<cmd>lua require('telescope.builtin').live_grep()<cr>",                "Grep files" },
  ["<leader>H"]       = { "<cmd>lua require('telescope.builtin').jumplist()<cr>",                 "Search history" },
  ["<leader>h"]       = { name = "Git hunk" },
  ["<leader>hb"]      = { "<cmd>lua require('gitsigns').blame_line({full = true})<cr>",           "Blame line" },
  ["<leader>hp"]      = { "<cmd>lua require('gitsigns').preview_hunk()<cr>",                      "Preview hunk" },
  ["<leader>hr"]      = { "<cmd>lua require('gitsigns').reset_hunk()<cr>",                        "Reset hunk" },
  ["<leader>hR"]      = { "<cmd>lua require('gitsigns').reset_buffer()<cr>",                      "Reset buffer" },
  ["<leader>hs"]      = { "<cmd>lua require('gitsigns').stage_hunk()<cr>",                        "Stage hunk" },
  ["<leader>hS"]      = { "<cmd>lua require('gitsigns').stage_buffer()<cr>",                      "Stage buffer" },
  ["<leader>hu"]      = { "<cmd>lua require('gitsigns').undo_stage_hunk()<cr>",                   "Undo stage hunk" },
  ["<leader>m"]       = { "<cmd>lua require('telescope.builtin').marks()<cr>",                    "Search marks" },
  ["<leader>r"]       = { "<cmd>lua require('telescope.builtin').registers()<cr>",                "Search registers" },
  ["<leader>t"]       = { "<cmd>lua require('telescope.builtin').tags()<cr>",                     "Search tags" },
  ["<leader>g"]       = { name = "Git" },
  ["<leader>gB"]      = { ":Git blame<cr>",                                                       "Blame" },
  ["<leader>gb"]      = { "<cmd>lua require('telescope.builtin').git_branches()<cr>",             "List branches" },
  ["<leader>gd"]      = { "<cmd>lua require('gitsigns').diffthis()<cr>",                          "Diff this" },
  ["<leader>gc"]      = { ":Git commit<cr>",                                                      "Commit" },
  ["<leader>gC"]      = { "<cmd>lua open_on_other_branch()<cr>",                                  "Open on other branch" },
  ["<leader>gg"]      = { ":G<cr><c-w>10_",                                                       "Fugitive status" },
  ["<leader>gL"]      = { "<cmd>lua require('telescope.builtin').git_bcommits()<cr>",             "List commits for buffer" },
  ["<leader>gl"]      = { "<cmd>lua require('telescope.builtin').git_commits()<cr>",              "List commits" },
  ["<leader>gp"]      = { ":G pull<cr>",                                                          "Pull" },
  ["<leader>gP"]      = { ":G push<cr>",                                                          "Push" },
  ["<leader>gQ"]      = { "<cmd>lua require('gitsigns').setloclist()<cr>",                        "Local list" },
  ["<leader>gq"]      = { "<cmd>lua require('gitsigns').setqflist()<cr>",                         "Quickfix list" },
  ["<leader>gS"]      = { "<cmd>lua require('telescope.builtin').git_stash()<cr>",                "Show stash" },
  ["<leader>gs"]      = { "<cmd>lua require('telescope.builtin').git_status()<cr>",               "Show status" },

  ["<localleader>,"]  = { "<cmd>lua vim.lsp.buf.hover()<cr>",                                     "Show documentation" },
  ["<localleader>a"]  = { "<cmd>lua vim.lsp.buf.code_action()<cr>",                               "Code actions" },
  ["<localleader>d"]  = { "<cmd>lua require('telescope.builtin').diagnostics()<cr>",              "Diagnostics" },
  ["<localleader>D"]  = { "<cmd>lua vim.diagnostic.setloclist({open_loclist = false})<cr>",       "Load diagnostics to loclist" },
  ["<localleader>f"]  = { "<cmd>lua vim.lsp.buf.format({async = true})<cr>",                      "Format" },
  ["<localleader>h"]  = { "<cmd>LspStart hls<cr>",                                                "LSP start HLS" },
  ["<localleader>i"]  = { "<cmd>lua require('telescope.builtin').lsp_incoming_calls(cth)<cr>",    "Incoming calls" },
  ["<localleader>I"]  = { "<cmd>LspInfo<cr>",                                                     "LSP info" },
  ["<localleader>l"]  = { "<cmd>lua vim.lsp.codelens.refresh()<cr>",                              "Code lens" },
  ["<localleader>o"]  = { "<cmd>lua require('telescope.builtin').lsp_outgoing_calls(cth)<cr>",    "Outgoing calls" },
  ["<localleader>r"]  = { "<cmd>lua require('telescope.builtin').lsp_references(cth)<cr>",        "List references" },
  ["<localleader>s"]  = { "<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<cr>",    "List workspace symbols" },

  ["<space>"]         = { name = "Harpoon" },
  ["<space><space>"]  = { "<cmd>Telescope harpoon marks<cr>",                                     "List" },
  ["<space><cr>"]     = { "<cmd>lua require('harpoon.mark').toggle_file()<cr>",                   "Add or reomove file" },
  ["<space><left>"]   = { "<cmd>lua require('harpoon.ui').nav_prev() <cr>",                       "Previous file" },
  ["<space><right>"]  = { "<cmd>lua require('harpoon.ui').nav_next() <cr>",                       "Next file" },
  ["<space>q"]        = { "<cmd>lua require('harpoon.ui').nav_file(1)<cr>",                       "File 1" },
  ["<space>w"]        = { "<cmd>lua require('harpoon.ui').nav_file(2)<cr>",                       "File 2" },
  ["<space>e"]        = { "<cmd>lua require('harpoon.ui').nav_file(3)<cr>",                       "File 3" },
  ["<space>r"]        = { "<cmd>lua require('harpoon.ui').nav_file(4)<cr>",                       "File 4" },
  ["<space>t"]        = { "<cmd>lua require('harpoon.ui').nav_file(5)<cr>",                       "File 5" }
}, { mode = "n" })

wk.register({
  ["<leader>a"]       = { name = "Tabularize" },
  ["<leader>a="]      = { ":Tab /^[^=]*\\zs=/l1c1l0<cr>",                                         "Align to '=' symbol" },
  ["<leader>a<bar>"]  = { ":Tab /|<cr>",                                                          "Align markdown table" },
  ["<leader>a:"]      = { ":Tab /^[^:]*\\zs:/l1c0l0<cr>",                                         "Align to first symbol" },
  ["<leader>a;"]      = { ":Tab /^[^:]*:\zs/l1l0<cr>",                                            "Align to key in hash" },
  ["<leader>at"]      = { ":Tabularize /",                                                        "Custom alignment", silent = false }
}, { mode = "v" })
