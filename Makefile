LN = ln -s
MK = mkdir -p
RM = rm -fr
APT = apt

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

install_ubuntu: clean_ubuntu dependencies_ubuntu
	${MK} ${HOME}/.local/bin
	${LN} ${PWD}/.local/bin/pbcopy ${HOME}/.local/bin/pbcopy
	${LN} ${PWD}/.local/bin/pbpaste ${HOME}/.local/bin/pbpaste
	${LN} ${PWD}/.local/bin/open ${HOME}/.local/bin/open

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

clean_ubuntu:
	${RM} ${HOME}/.local/bin/projects \
	      ${HOME}/.local/bin/pbcopy \
	      ${HOME}/.local/bin/pbpaste \
	      ${HOME}/.local/bin/open

dependencies_ubuntu:
	${APT} update
	${APT} upgrade
	${APT} install bash git tmux vim xclip silversearcher-ag exuberant-ctags entr
