require 'ap'
require 'pp'
require 'rb-readline'
require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:USE_READLINE] = true
IRB.conf[:PROMPT_MODE]  = :SIMPLE
IRB.conf[:AUTO_INDENT]=true
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"

def RbReadline.rl_reverse_search_history(sign, key)
  rl_insert_text  `cat ~/.pry_history ~/.irb_history | fzf --tac |  tr '\n' ' '`
end
