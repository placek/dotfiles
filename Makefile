LN = ln -s
MK = mkdir -p
RM = rm -fr
APT = apt
UNAME := $(shell uname -s)

install: clean
	${MK} ${HOME}/.local/bin
	${LN} ${PWD}/.bash_plugins ${HOME}
	${LN} ${PWD}/.git_template ${HOME}
	${LN} ${PWD}/.vim ${HOME}
	${LN} ${PWD}/.bash_profile ${HOME}
	${LN} ${PWD}/.gitconfig ${HOME}
	${LN} ${PWD}/.gitignore_global ${HOME}
	${LN} ${PWD}/.inputrc ${HOME}
	${LN} ${PWD}/.tmux.conf ${HOME}
	${LN} ${PWD}/.vimrc ${HOME}
	${LN} ${FLAGS} ${PWD}/.local/bin/projects ${HOME}/.local/bin/projects
	${MK} ${HOME}/.local/bin
	${LN} ${PWD}/.local/bin/pbcopy ${HOME}/.local/bin/pbcopy
	${LN} ${PWD}/.local/bin/pbpaste ${HOME}/.local/bin/pbpaste
	${LN} ${PWD}/.local/bin/open ${HOME}/.local/bin/open

clean:
	${RM} ${HOME}/.local/bin/projects \
	      ${HOME}/.bash_plugins \
	      ${HOME}/.git_template \
	      ${HOME}/.vim \
	      ${HOME}/.bash_profile \
	      ${HOME}/.gitconfig \
	      ${HOME}/.gitignore_global \
	      ${HOME}/.inputrc \
	      ${HOME}/.tmux.conf \
	      ${HOME}/.vimrc \
	      ${HOME}/.local/bin/pbcopy \
	      ${HOME}/.local/bin/pbpaste \
	      ${HOME}/.local/bin/open
