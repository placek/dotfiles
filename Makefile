CP  = cp
LN  = ln -s
MK  = mkdir -p
RM  = rm -fr
GIT = git

.PHONY: clean install nix wall

install: clean
	${MK} ${HOME}/Downloads
	${MK} ${HOME}/Music
	${MK} ${HOME}/Projects
	${GIT} ls-files | xargs -n 1 dirname | uniq | xargs -I@ ${MK} ${HOME}/@
	${GIT} ls-files  | grep -Ev ".vim/|Makefile|LICENSE|README|configuration.nix" | xargs -I@ ${LN} ${PWD}/@ ${HOME}/@

clean:
	${GIT} ls-files  | grep -Ev ".vim/|Makefile|LICENSE|README|configuration.nix" | xargs -I@ ${RM} ${HOME}/@

wall:
	${GIT} clone https://gitlab.com/dwt1/wallpapers.git .wall

nix:
	${CP} configuration.nix /etc/nixos/configuration.nix
	nixos-rebuild switch
