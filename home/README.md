# HOME

My `$HOME` configuration files stored in **gnustow** packages.

### installation

1. Copy this directory to the target configuration path (like `$HOME/.config/dotfiles`).
2. Run stow to link the files, like:
```
stow --dir=$HOME/.config/dotfiles --target=$HOME -S <package1> <package2> ...
```
