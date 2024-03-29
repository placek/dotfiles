local wk = require("which-key")
local cth = require("telescope.themes").get_cursor()

wk.register({
  ["<leader>/"]       = { "<cmd>lua require('telescope').extensions.file_browser.file_browser({ path = '%:p:h', grouped = true, hidden = true, respect_gitignore = true, display_stat = { date = true }})<cr>", "Browse files" },
  ["<leader><esc>"]   = { "<cmd>lua require('telescope.builtin').resume()<cr>",                   "Last search" },
  ["<leader><space>"] = { "<cmd>lua require('telescope.builtin').builtin()<cr>",                  "Other search options" },
  ["<leader>\\"]      = { "<cmd>lua require('telescope.builtin').git_files({hidden=true})<cr>",   "Search git files only" },
  ["<leader>b"]       = { "<cmd>lua require('telescope.builtin').buffers()<cr>",                  "Search buffers" },
  ["<leader>F"]       = { "<cmd>lua require('telescope.builtin').find_files()<cr>",               "Search files" },
  ["<leader>f"]       = { "<cmd>lua require('telescope.builtin').live_grep()<cr>",                "Grep files" },
  ["<leader>H"]       = { "<cmd>lua require('telescope.builtin').jumplist()<cr>",                 "Search history" },
  ["<leader>h"]       = { name = "Git hunk" },
  ["<leader>hp"]      = { "<cmd>lua require('gitsigns').preview_hunk()<cr>",                      "Preview" },
  ["<leader>hr"]      = { "<cmd>lua require('gitsigns').reset_hunk()<cr>",                        "Reset" },
  ["<leader>hR"]      = { "<cmd>lua require('gitsigns').reset_buffer()<cr>",                      "Reset buffer" },
  ["<leader>hs"]      = { "<cmd>lua require('gitsigns').stage_hunk()<cr>",                        "Stage" },
  ["<leader>hS"]      = { "<cmd>lua require('gitsigns').stage_buffer()<cr>",                      "Stage buffer" },
  ["<leader>hu"]      = { "<cmd>lua require('gitsigns').undo_stage_hunk()<cr>",                   "Undo stage" },
  ["<leader>m"]       = { "<cmd>lua require('telescope.builtin').marks()<cr>",                    "Search marks" },
  ["<leader>r"]       = { "<cmd>lua require('telescope.builtin').registers()<cr>",                "Search registers" },
  ["<leader>t"]       = { "<cmd>lua require('telescope.builtin').tags()<cr>",                     "Search tags" },
  ["<leader>u"]       = { "<cmd>UndotreeToggle<cr>",                                              "Undo tree" },
  ["<leader>g"]       = { name = "Git" },
  ["<leader>gB"]      = { "<cmd>Git blame<cr>",                                                   "Blame" },
  ["<leader>gb"]      = { "<cmd>lua require('telescope.builtin').git_branches()<cr>",             "Branches" },
  ["<leader>gd"]      = { "<cmd>lua require('gitsigns').diffthis()<cr>",                          "Diff this" },
  ["<leader>gc"]      = { "<cmd>Git commit<cr>",                                                  "Commit" },
  ["<leader>gC"]      = { "<cmd>lua open_on_other_branch()<cr>",                                  "Open on other branch" },
  ["<leader>gg"]      = { "<cmd>lua toggle_fugitive()<cr>",                                       "Git status" },
  ["<leader>gL"]      = { "<cmd>lua require('telescope.builtin').git_bcommits()<cr>",             "Commits for buffer" },
  ["<leader>gl"]      = { "<cmd>lua require('telescope.builtin').git_commits()<cr>",              "Commits" },
  ["<leader>gp"]      = { "<cmd>G pull<cr>",                                                      "Pull" },
  ["<leader>gP"]      = { "<cmd>G push<cr>",                                                      "Push" },
  ["<leader>gf"]      = { "<cmd>G pull -f<cr>",                                                   "Force pull" },
  ["<leader>gF"]      = { "<cmd>G push -f<cr>",                                                   "Force push" },
  ["<leader>ga"]      = { "<cmd>G commit --amend<cr>",                                            "Amend with new message" },
  ["<leader>gA"]      = { "<cmd>G amend<cr>",                                                     "Amend" },
  ["<leader>gQ"]      = { "<cmd>lua require('gitsigns').setloclist()<cr>",                        "Local list" },
  ["<leader>gq"]      = { "<cmd>lua require('gitsigns').setqflist()<cr>",                         "Quickfix list" },
  ["<leader>gS"]      = { "<cmd>lua require('telescope.builtin').git_stash()<cr>",                "Stash" },
  ["<leader>gs"]      = { "<cmd>lua require('telescope.builtin').git_status()<cr>",               "Status" },

  ["<localleader>,"]  = { "<cmd>lua vim.lsp.buf.hover()<cr>",                                     "Show documentation" },
  ["<localleader>."]  = { "<cmd>lua vim.diagnostic.open_float({ border = 'rounded' })<cr>",       "Show diagnostic" },
  ["<localleader>a"]  = { "<cmd>lua vim.lsp.buf.code_action()<cr>",                               "Code actions" },
  ["<localleader>l"]  = { "<cmd>lua vim.lsp.codelens.refresh()<cr>",                              "Code lens" },
  ["<localleader>d"]  = { "<cmd>lua require('telescope.builtin').diagnostics()<cr>",              "Diagnostics" },
  ["<localleader>D"]  = { "<cmd>lua vim.diagnostic.setloclist({ open_loclist = false })<cr>",     "Diagnostics in loclist" },
  ["<localleader>f"]  = { "<cmd>lua vim.lsp.buf.format({ async = true })<cr>",                    "Format" },
  ["<localleader>i"]  = { "<cmd>lua require('telescope.builtin').lsp_incoming_calls(cth)<cr>",    "Incoming calls" },
  ["<localleader>o"]  = { "<cmd>lua require('telescope.builtin').lsp_outgoing_calls(cth)<cr>",    "Outgoing calls" },
  ["<localleader>r"]  = { "<cmd>lua require('telescope.builtin').lsp_references(cth)<cr>",        "References" },
  ["<localleader>s"]  = { "<cmd>AerialToggle<cr>",                                                "Symbols outline" },
  ["<localleader>S"]  = { "<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<cr>",    "Workspace symbols" },
  ["<localleader>I"]  = { "<cmd>LspInfo<cr>",                                                     "LSP info" },
  ["<localleader>H"]  = { "<cmd>LspStart hls<cr>",                                                "LSP start HLS" },

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
  ["<leader>a="]      = { "<cmd>Tab /^[^=]*\\zs=/l1c1l0<cr>",                                     "Align to '=' symbol" },
  ["<leader>a<bar>"]  = { "<cmd>Tab /|<cr>",                                                      "Align markdown table" },
  ["<leader>a:"]      = { "<cmd>Tab /^[^:]*\\zs:/l1c0l0<cr>",                                     "Align to first symbol" },
  ["<leader>as"]      = { "<cmd>Tab /::<cr>",                                                     "Align to '::'" },
  ["<leader>a;"]      = { "<cmd>Tab /^[^:]*:\zs/l1l0<cr>",                                        "Align to key in hash" },
  ["<leader>at"]      = { ":Tabularize /",                                                        "Custom alignment", silent = false }
}, { mode = "v" })
