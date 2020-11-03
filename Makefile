LN  = ln -s
MK  = mkdir -p
RM  = rm -fr
NO  = yes n
GIT = git

.PHONY: clean install

install: clean
	${MK} ${HOME}/.local/bin
	${MK} ${HOME}/.xmonad
	${MK} ${HOME}/.config/rofi
	${MK} ${HOME}/.wall
	${LN} ${PWD}/.bash ${HOME}
	${LN} ${PWD}/.git_template ${HOME}
	${LN} ${PWD}/.vim ${HOME}
	${LN} ${PWD}/.bash_profile ${HOME}
	${LN} ${PWD}/.bash_profile ${HOME}/.bashrc
	${LN} ${PWD}/.gitconfig ${HOME}
	${LN} ${PWD}/.gitignore_global ${HOME}
	${LN} ${PWD}/.inputrc ${HOME}
	${LN} ${PWD}/.tmux.conf ${HOME}
	${LN} ${PWD}/.vimrc ${HOME}
	${LN} ${PWD}/.xmobarrc ${HOME}
	${LN} ${PWD}/.Xresources ${HOME}
	${LN} ${PWD}/.wall/default.jpg ${HOME}/.wall/default.jpg
	${LN} ${PWD}/.config/rofi/config.rasi ${HOME}/.config/rofi/config.rasi
	${LN} ${PWD}/.xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	${LN} ${PWD}/.local/bin/projects ${HOME}/.local/bin/projects
	${MK} ${HOME}/.local/bin
	${LN} ${PWD}/.local/bin/pbcopy ${HOME}/.local/bin/pbcopy
	${LN} ${PWD}/.local/bin/pbpaste ${HOME}/.local/bin/pbpaste
	${LN} ${PWD}/.local/bin/open ${HOME}/.local/bin/open

clean:
	${RM} ${HOME}/.local/bin/projects \
	      ${HOME}/.config/rofi/config.rasi \
	      ${HOME}/.wall/* \
	      ${HOME}/.xmonad/xmonad.hs \
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
	      ${HOME}/.xmobarrc \
	      ${HOME}/.Xresources \
	      ${HOME}/.local/bin/pbcopy \
	      ${HOME}/.local/bin/pbpaste \
	      ${HOME}/.local/bin/open
