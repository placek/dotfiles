# dotfiles

My `$HOME` configuration files stored in **gnustow** packages.

### installation

1. Copy this directory to the target configuration path (like `$HOME/.config/dotfiles`).
2. Run stow to link the files, like:
```
stow --dir=$HOME/.config/dotfiles --target=$HOME -S <package1> <package2> ...
```

### contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### license

Distributed under the MIT License. See `LICENSE` for more information.
