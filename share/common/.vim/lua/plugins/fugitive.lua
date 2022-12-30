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
