# dotfiles

This is a set of configuration files I use daily.

## Getting Started

### Prerequisites

To fully use the configuration you need to install corresponding software:

```sh
nix-env -i bash curl git tmux vim tig silver-searcher ctags entr
nix-env -i xmonad xmobar rofi keepassxc google-chrome rxvt-unicode xclip dunst feh
```

### Installation

1. Clone the repo:
```sh
git clone https://github.com/placek/dotfiles.git
```
2. Make a custom branch:
```sh
git checkout -B my-configuration
```
3. Run make:
```sh
make install
```
3. Feel free to edit specific entries and commit them:
```sh
vim <whatever-config-file-in-project>
git add .
git commit -m "my specific configuration"
```
...or use them "as is".

## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

Distributed under the MIT License. See `LICENSE` for more information.
