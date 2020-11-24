CP  = cp
LN  = ln -s
MK  = mkdir -p
RM  = rm -fr

.PHONY: clean install nix

install: clean
	${MK} ${HOME}/Downloads
	${MK} ${HOME}/Music
	${MK} ${HOME}/Projects
	${MK} ${HOME}/.config/dunst
	${MK} ${HOME}/.config/fish
	${MK} ${HOME}/.config/termonad
	${MK} ${HOME}/.config/qutebrowser
	${MK} ${HOME}/.config/rofi
	${MK} ${HOME}/.local/bin
	${MK} ${HOME}/.moc/themes
	${MK} ${HOME}/.mutt/com.binarapps.p.placzynski/headers
	${MK} ${HOME}/.mutt/com.binarapps.p.placzynski/messages
	${MK} ${HOME}/.mutt/com.gmail.placzynski.pawel/headers
	${MK} ${HOME}/.mutt/com.gmail.placzynski.pawel/messages
	${MK} ${HOME}/.mutt/com.gmail.silquenarmo/headers
	${MK} ${HOME}/.mutt/com.gmail.silquenarmo/messages
	${MK} ${HOME}/.wall
	${MK} ${HOME}/.xmonad
	${LN} ${PWD}/.mutt/com.gmail.silquenarmo/account ${HOME}/.mutt/com.gmail.silquenarmo/account
	${LN} ${PWD}/.mutt/com.gmail.placzynski.pawel/account ${HOME}/.mutt/com.gmail.placzynski.pawel/account
	${LN} ${PWD}/.mutt/com.binarapps.p.placzynski/account ${HOME}/.mutt/com.binarapps.p.placzynski/account
	${LN} ${PWD}/.Xresources ${HOME}
	${LN} ${PWD}/.bash_profile ${HOME}
	${LN} ${PWD}/.config/dunst/dunstrc ${HOME}/.config/dunst/dunstrc
	${LN} ${PWD}/.config/greenclip.cfg ${HOME}/.config/greenclip.cfg
	${LN} ${PWD}/.config/fish/base16-flat.fish ${HOME}/.config/fish/base16-flat.fish
	${LN} ${PWD}/.config/fish/config.fish ${HOME}/.config/fish/config.fish
	${LN} ${PWD}/.config/fish/prompt.fish ${HOME}/.config/fish/prompt.fish
	${LN} ${PWD}/.config/termonad/termonad.hs ${HOME}/.config/termonad/termonad.hs
	${LN} ${PWD}/.config/qutebrowser/config.py ${HOME}/.config/qutebrowser/config.py
	${LN} ${PWD}/.config/rofi/config.rasi ${HOME}/.config/rofi/config.rasi
	${LN} ${PWD}/.git_template ${HOME}
	${LN} ${PWD}/.gitconfig ${HOME}
	${LN} ${PWD}/.gitignore_global ${HOME}
	${LN} ${PWD}/.irbrc ${HOME}
	${LN} ${PWD}/.local/bin/open ${HOME}/.local/bin/open
	${LN} ${PWD}/.local/bin/pbcopy ${HOME}/.local/bin/pbcopy
	${LN} ${PWD}/.local/bin/pbpaste ${HOME}/.local/bin/pbpaste
	${LN} ${PWD}/.local/bin/projects ${HOME}/.local/bin/projects
	${LN} ${PWD}/.local/bin/sc ${HOME}/.local/bin/sc
	${LN} ${PWD}/.logo.xpm ${HOME}
	${LN} ${PWD}/.mailcap ${HOME}
	${LN} ${PWD}/.muttrc ${HOME}
	${LN} ${PWD}/.moc/config ${HOME}/.moc/config
	${LN} ${PWD}/.moc/themes/dylanwh ${HOME}/.moc/themes/dylanwh
	${LN} ${PWD}/.tmux.conf ${HOME}
	${LN} ${PWD}/.vim ${HOME}
	${LN} ${PWD}/.vimrc ${HOME}
	${LN} ${PWD}/.xmobarrc ${HOME}
	${LN} ${PWD}/.xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs

clean:
	${RM} ${HOME}/.Xresources \
	      ${HOME}/.bash_profile \
	      ${HOME}/.config/dunst/dunstrc \
	      ${HOME}/.config/fish/base16-flat.fish \
	      ${HOME}/.config/fish/config.fish \
	      ${HOME}/.config/fish/prompt.fish \
	      ${HOME}/.config/greenclip.cfg \
	      ${HOME}/.config/qutebrowser/config.py \
	      ${HOME}/.config/rofi/config.rasi \
	      ${HOME}/.config/termonad/termonad.hs \
	      ${HOME}/.git_template \
	      ${HOME}/.gitconfig \
	      ${HOME}/.gitignore_global \
	      ${HOME}/.irbrc \
	      ${HOME}/.local/bin/open \
	      ${HOME}/.local/bin/pbcopy \
	      ${HOME}/.local/bin/pbpaste \
	      ${HOME}/.local/bin/projects \
	      ${HOME}/.local/bin/sc \
	      ${HOME}/.logo.xpm \
	      ${HOME}/.mailcap \
	      ${HOME}/.moc/config \
	      ${HOME}/.moc/themes/dylanwh \
	      ${HOME}/.mutt/**/account \
	      ${HOME}/.muttrc \
	      ${HOME}/.tmux.conf \
	      ${HOME}/.vim \
	      ${HOME}/.vimrc \
	      ${HOME}/.xmobarrc \
	      ${HOME}/.xmonad/xmonad.hs

nix:
	${CP} configuration.nix /etc/nixos/configuration.nix
	nixos-rebuild switch
