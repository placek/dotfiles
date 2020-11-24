CP  = cp
LN  = ln -s
MK  = mkdir -p
RM  = rm -fr
ARG = xargs -I@
EXC = grep -Ev ".vim/|Makefile|LICENSE|README|configuration.nix"

.PHONY: clean install nix wall

install: clean
	${MK} ${HOME}/Downloads
	${MK} ${HOME}/Music
	${MK} ${HOME}/Projects
	${GIT} ls-files | ${ARG} dirname @ | sort -u | ${ARG} ${MK} ${HOME}/@
	${GIT} ls-files  | ${EXC} | ${ARG} ${LN} ${PWD}/@ ${HOME}/@

clean:
	${GIT} ls-files  | ${EXC} | ${ARG} ${RM} ${HOME}/@

wall:
	${GIT} clone https://gitlab.com/dwt1/wallpapers.git .wall

nix:
	${CP} configuration.nix /etc/nixos/configuration.nix
	nixos-rebuild switch
