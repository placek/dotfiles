local function show_fugitive()
  if vim.fn.FugitiveHead() ~= '' then
    vim.cmd [[
    Git
    resize 10
    setlocal winfixheight
    setlocal nonumber
    setlocal norelativenumber
    ]]
  end
end

toggle_fugitive = function()
  if vim.fn.buflisted(vim.fn.bufname('fugitive:///*/.git//$')) ~= 0 then
    vim.cmd [[ execute ":bdelete" bufname('fugitive:///*/.git//$') ]]
  else
    show_fugitive()
  end
end

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
