install:
	ln -sv ${PWD}/.bash_plugins ~
	ln -sv ${PWD}/.git_template ~
	ln -sv ${PWD}/.vim ~
	ln -sv ${PWD}/.bash_profile ~
	ln -sv ${PWD}/.gitconfig ~
	ln -sv ${PWD}/.gitignore_global ~
	ln -sv ${PWD}/.inputrc ~
	ln -sv ${PWD}/.tmux.conf ~
	ln -sv ${PWD}/.vimrc ~

clean:
	rm -rf ~/.bash_plugins
	rm -rf ~/.git_template
	rm -rf ~/.vim
	rm -rf ~/.bash_profile
	rm -rf ~/.gitconfig
	rm -rf ~/.gitignore_global
	rm -rf ~/.inputrc
	rm -rf ~/.tmux.conf
	rm -rf ~/.vimrc
