parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1/'
}

parse_git_status() {
  [ -n "$(git status --porcelain 2> /dev/null)" ] && echo "*"
}

PROMPT_COMMAND=__set_prompt

__set_prompt() {
  local exit=$?
  local cl_nrm="\[$(tput sgr0)\]"
  local cl_blk="\[$(tput setaf 4)\]"
  local cl_red="\[$(tput setaf 1)\]"
  local cl_grn="\[$(tput setaf 2)\]"
  local cl_org="\[$(tput setaf 3)\]"
  PS1="${cl_blk}\w${cl_nrm}${cl_org}\$(parse_git_branch)\$(parse_git_status)${cl_nrm} "
  if [[ $exit -eq 0 ]]; then
    PS1+="${cl_grn}$ ${cl_nrm}"
  else
    PS1+="${cl_red}$ ${cl_nrm}"
  fi
}

