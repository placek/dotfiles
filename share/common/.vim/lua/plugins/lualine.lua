require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "gruvbox-material",
    component_separators = { left = "", right = ""},
    section_separators = { left = "", right = ""},
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
