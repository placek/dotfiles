LN  = ln -s
MK  = mkdir -p
RM  = rm -fr
GIT = git
UNAME := $(shell uname -s)

.PHONY: clean symlink install

clean:
	${RM} ${HOME}/.local/bin/projects \
	      ${HOME}/.bash \
	      ${HOME}/.git_template \
	      ${HOME}/.vim \
	      ${HOME}/.bash_profile \
	      ${HOME}/.bashrc \
	      ${HOME}/.gitconfig \
	      ${HOME}/.gitignore_global \
	      ${HOME}/.inputrc \
	      ${HOME}/.tmux.conf \
	      ${HOME}/.vimrc \
	      ${HOME}/.wall.jpg \
	      ${HOME}/.local/bin/pbcopy \
	      ${HOME}/.local/bin/pbpaste \
	      ${HOME}/.local/bin/open

symlink: clean
	${MK} ${HOME}/.local/bin
	${LN} ${PWD}/.bash ${HOME}
	${LN} ${PWD}/.git_template ${HOME}
	${LN} ${PWD}/.vim ${HOME}
	${LN} ${PWD}/.bash_profile ${HOME}
	${LN} ${PWD}/.bashrc ${HOME}
	${LN} ${PWD}/.gitconfig ${HOME}
	${LN} ${PWD}/.gitignore_global ${HOME}
	${LN} ${PWD}/.inputrc ${HOME}
	${LN} ${PWD}/.tmux.conf ${HOME}
	${LN} ${PWD}/.vimrc ${HOME}
	${LN} ${PWD}/.wall.jpg ${HOME}
	${LN} ${FLAGS} ${PWD}/.local/bin/projects ${HOME}/.local/bin/projects
	${MK} ${HOME}/.local/bin
	${LN} ${PWD}/.local/bin/pbcopy ${HOME}/.local/bin/pbcopy
	${LN} ${PWD}/.local/bin/pbpaste ${HOME}/.local/bin/pbpaste
	${LN} ${PWD}/.local/bin/open ${HOME}/.local/bin/open

install: symlink
	${GIT} submodule init
	${GIT} submodule update
	.vim/pack/bundle/opt/fzf/install
