local function showFugitiveGit()
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

function toggleFugitiveGit()
  if vim.fn.buflisted(vim.fn.bufname('fugitive:///*/.git//$')) ~= 0 then
    vim.cmd [[ execute ":bdelete" bufname('fugitive:///*/.git//$') ]]
  else
    showFugitiveGit()
  end
end
