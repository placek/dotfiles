LN = ln
FLAGS = -s

install: clean
	${LN} ${FLAGS} ${PWD}/.bash_plugins ${HOME}
	${LN} ${FLAGS} ${PWD}/.git_template ${HOME}
	${LN} ${FLAGS} ${PWD}/.vim ${HOME}
	${LN} ${FLAGS} ${PWD}/.bash_profile ${HOME}
	${LN} ${FLAGS} ${PWD}/.gitconfig ${HOME}
	${LN} ${FLAGS} ${PWD}/.gitignore_global ${HOME}
	${LN} ${FLAGS} ${PWD}/.inputrc ${HOME}
	${LN} ${FLAGS} ${PWD}/.tmux.conf ${HOME}
	${LN} ${FLAGS} ${PWD}/.vimrc ${HOME}
	mkdir -p ${HOME}/.local/bin
	${LN} ${FLAGS} ${PWD}/.local/bin/projects ${HOME}/.local/bin/projects

clean:
	rm -rf ${HOME}/.bash_plugins \
	       ${HOME}/.git_template \
	       ${HOME}/.vim \
	       ${HOME}/.bash_profile \
	       ${HOME}/.gitconfig \
	       ${HOME}/.gitignore_global \
	       ${HOME}/.inputrc \
	       ${HOME}/.tmux.conf \
	       ${HOME}/.vimrc
