LN = ln -s
MK = mkdir -p
RM = rm -fr

install: clean
	${LN} ${PWD}/.bash_plugins ${HOME}
	${LN} ${PWD}/.git_template ${HOME}
	${LN} ${PWD}/.vim ${HOME}
	${LN} ${PWD}/.bash_profile ${HOME}
	${LN} ${PWD}/.gitconfig ${HOME}
	${LN} ${PWD}/.gitignore_global ${HOME}
	${LN} ${PWD}/.inputrc ${HOME}
	${LN} ${PWD}/.tmux.conf ${HOME}
	${LN} ${PWD}/.vimrc ${HOME}
	${MK} ${HOME}/.local/bin
	${LN} ${FLAGS} ${PWD}/.local/bin/projects ${HOME}/.local/bin/projects

install_linux: clean_linux
	${MK} ${HOME}/.local/bin
	${MK} ${HOME}/.config/dunst
	${MK} ${HOME}/.config/systemd/user
	${LN} ${PWD}/.local/bin/pbcopy ${HOME}/.local/bin/pbcopy
	${LN} ${PWD}/.local/bin/pbpaste ${HOME}/.local/bin/pbpaste
	${LN} ${PWD}/.local/bin/open ${HOME}/.local/bin/open
	${LN} ${PWD}/.config/dunst/dunstrc ${HOME}/.config/dunst/dunstrc
	${LN} ${PWD}/.config/systemd/user/ssh-agent.service ${HOME}/.config/systemd/user/ssh-agent.service

clean:
	${RM} ${HOME}/.bash_plugins \
	      ${HOME}/.git_template \
	      ${HOME}/.vim \
	      ${HOME}/.bash_profile \
	      ${HOME}/.gitconfig \
	      ${HOME}/.gitignore_global \
	      ${HOME}/.inputrc \
	      ${HOME}/.tmux.conf \
	      ${HOME}/.vimrc

clean_linux:
	${RM} ${HOME}/.local/bin/pbcopy \
	      ${HOME}/.local/bin/pbpaste \
	      ${HOME}/.local/bin/open \
	      ${HOME}/.config/dunst/dunsrc \
	      ${HOME}/.config/systemd/user/ssh-agent.service
