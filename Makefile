GIT   ?= git
XARGS ?= xargs
GREP  ?= grep
EXP    = -Ev "Makefile|LICENSE|README|configuration.nix"

.PHONY: clean install nix wall

install: clean
	mkdir -p ${HOME}/Downloads
	mkdir -p ${HOME}/Music
	mkdir -p ${HOME}/Projects
	${GIT} ls-files | ${XARGS} -I@ dirname @ | sort -u | ${XARGS} -I@ mkdir -p ${HOME}/@
	${GIT} ls-files  | ${GREP} ${EXP} | ${XARGS} -I@ ln -Ffs ${PWD}/@ ${HOME}/@

clean:
	${GIT} ls-files  | ${GREP} ${EXP} | ${XARGS} -I@ rm -fr ${HOME}/@

wall:
	${GIT} clone https://gitlab.com/dwt1/wallpapers.git .wall

nix:
	cp configuration.nix /etc/nixos/configuration.nix
	nixos-rebuild switch
