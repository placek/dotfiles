require("plugins.lualine")
require("plugins.telescope")
require("plugins.cmp")
require("plugins.comment")
require("plugins.fidget")
require("plugins.fugitive")
require("plugins.gitsigns")
require("plugins.lsp")
require("plugins.whichkey")
require("plugins.zen")

open_on_other_branch = function()
  local command = "git for-each-ref --format='%(refname:short)' refs/heads/"
  local result = vim.fn.system(command)
  local branches = {}
  for s in result:gmatch("[^\r\n]+") do
    table.insert(branches, s)
  end
  vim.ui.select(branches, { prompt = "Select branch:" }, function(branch, i)
    if branch then
      vim.cmd(":Gvsplit " .. branch .. ":%")
    end
  end)
end
