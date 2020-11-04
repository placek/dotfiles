LN  = ln -s
MK  = mkdir -p
RM  = rm -fr
NO  = yes n
GIT = git

.PHONY: clean install

install: clean
	${LN} ${PWD}/.Xresources ${HOME}
	${LN} ${PWD}/.bash ${HOME}
	${LN} ${PWD}/.bash_profile ${HOME}
	${LN} ${PWD}/.config/dunst/dunstrc ${HOME}/.config/dunst/dunstrc
	${LN} ${PWD}/.config/rofi/config.rasi ${HOME}/.config/rofi/config.rasi
	${LN} ${PWD}/.git_template ${HOME}
	${LN} ${PWD}/.gitconfig ${HOME}
	${LN} ${PWD}/.gitignore_global ${HOME}
	${LN} ${PWD}/.irbrc ${HOME}
	${LN} ${PWD}/.local/bin/open ${HOME}/.local/bin/open
	${LN} ${PWD}/.local/bin/pbcopy ${HOME}/.local/bin/pbcopy
	${LN} ${PWD}/.local/bin/pbpaste ${HOME}/.local/bin/pbpaste
	${LN} ${PWD}/.local/bin/projects ${HOME}/.local/bin/projects
	${LN} ${PWD}/.tmux.conf ${HOME}
	${LN} ${PWD}/.vim ${HOME}
	${LN} ${PWD}/.vimrc ${HOME}
	${LN} ${PWD}/.wall/default.jpg ${HOME}/.wall/default.jpg
	${LN} ${PWD}/.xmobarrc ${HOME}
	${LN} ${PWD}/.xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	${MK} ${HOME}/.config/dunst
	${MK} ${HOME}/.config/rofi
	${MK} ${HOME}/.local/bin
	${MK} ${HOME}/.local/bin
	${MK} ${HOME}/.wall
	${MK} ${HOME}/.xmonad

clean:
	${RM} ${HOME}/.Xresources \
	      ${HOME}/.bash \
	      ${HOME}/.bash_profile \
	      ${HOME}/.config/dunst/dunstrc \
	      ${HOME}/.config/rofi/config.rasi \
	      ${HOME}/.git_template \
	      ${HOME}/.gitconfig \
	      ${HOME}/.gitignore_global \
	      ${HOME}/.irbrc \
	      ${HOME}/.local/bin/open \
	      ${HOME}/.local/bin/pbcopy \
	      ${HOME}/.local/bin/pbpaste \
	      ${HOME}/.local/bin/projects \
	      ${HOME}/.tmux.conf \
	      ${HOME}/.vim \
	      ${HOME}/.vimrc \
	      ${HOME}/.wall/* \
	      ${HOME}/.xmobarrc \
	      ${HOME}/.xmonad/xmonad.hs
