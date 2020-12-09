{ config, pkgs, ... }:

{
  # imports
  imports = [
    ./hardware-configuration.nix
  ];

  # common setup
  boot.cleanTmpDir                         = true;
  console.keyMap                           = "pl";
  fonts.enableDefaultFonts                 = true;
  fonts.fontconfig.defaultFonts.monospace  = [ "Iosevka" ];
  fonts.fontconfig.defaultFonts.sansSerif  = [ "Ubuntu" ];
  fonts.fontconfig.defaultFonts.serif      = [ "Ubuntu" ];
  fonts.fonts                              = [ pkgs.iosevka-bin pkgs.ubuntu_font_family ];
  hardware.bluetooth.enable                = true;
  hardware.pulseaudio.enable               = true;
  i18n.defaultLocale                       = "pl_PL.UTF-8";
  networking.firewall.allowPing            = false;
  networking.firewall.allowedTCPPortRanges = [ { from = 3000; to = 3009; } ];
  networking.firewall.enable               = true;
  networking.hostName                      = "vm-nixos";
  networking.networkmanager.enable         = true;
  powerManagement.enable                   = true;
  programs.gnupg.agent.enable              = true;
  programs.gnupg.agent.enableSSHSupport    = true;
  programs.ssh.startAgent                  = false;
  security.wrappers.slock.source           = "${pkgs.slock.out}/bin/slock";
  sound.enable                             = true;
  system.autoUpgrade.allowReboot           = true;
  system.autoUpgrade.channel               = https://nixos.org/channels/nixos-20.09;
  system.autoUpgrade.enable                = true;
  system.stateVersion                      = "20.09";
  time.timeZone                            = "Europe/Warsaw";
  virtualisation.docker.autoPrune.dates    = "daily";
  virtualisation.docker.enable             = true;

  # boot loader
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    enable  = true;
    version = 2;
    device  = "/dev/sda";
  };

  # software
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    (pass.withExtensions (ext: with ext; [pass-otp pass-import]))
    (weechat.override {
      configure = {availablePlugins, ...}: {
        plugins = with availablePlugins; [ python perl ];
        scripts = with pkgs.weechatScripts; [ weechat-notify-send wee-slack ];
        init = ''
          /script install vimode.py
          /set weechat.bar.buflist.size_max 30
          /set weechat.bar.input.items "mode_indicator+[input_prompt]+(away),[input_search], [input_paste],input_text,[vi_buffer]"
          /set weechat.bar.vi_line_numbers.hidden off
          /mouse enable
        '';
      };
    })
    bash
    bat
    ctags
    curl
    docker-compose
    entr
    fd
    fish
    fzf
    git
    gnumake
    inxi
    libnotify
    moc
    ncdu
    neomutt
    openvpn
    paperkey
    pinentry-curses
    rclone
    rsync
    silver-searcher
    stow
    tig
    tmux
    ((vim_configurable.override { python = python3; }).customize {
      name = "vim";
      vimrcConfig.customRC = ''
        filetype plugin indent on
        syntax enable

        " settings
        set backspace=indent,eol,start
        set backup
        set backupdir=/tmp
        set clipboard=unnamedplus
        set completeopt=longest,menuone
        set cursorline
        set dir=/tmp
        set expandtab
        set foldmethod=manual
        set foldcolumn=1
        set formatoptions=tcrqn
        set grepformat=%f:%l:%c:%m
        set grepprg=ag\ --vimgrep\ $*
        set hlsearch
        set incsearch
        set laststatus=2
        set listchars=tab:→\ ,space:·,eol:¬,nbsp:◦
        set mouse=a
        set nocompatible
        set noshowmode
        set nospell
        set number
        set omnifunc=syntaxcomplete#Complete
        set path+=**
        set relativenumber
        set shiftwidth=2
        set showcmd
        set showtabline=2
        set softtabstop=2
        set splitbelow
        set splitright
        set swapfile
        set tabstop=2
        set tags+=.git/tags;
        set term=screen-256color
        set timeoutlen=1000 ttimeoutlen=0
        set ttyfast
        set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.rar,*.zip,*.tar.*,*.bmp,*.jpeg,*.avi,*.mov,*.mp7,*.ogg,*.flac
        set wrapmargin=0
        set wildmenu

        " mapping
        nnoremap <Leader>v :vsplit<CR>
        nnoremap <Leader>s :split<CR>
        nnoremap <Leader>1 :set number!<CR>
        nnoremap <Leader>2 :set relativenumber!<CR>
        nnoremap <Leader>3 :GitGutterToggle<CR>
        nnoremap <Leader>4 :set hlsearch!<CR>
        nnoremap <Leader>5 :set list!<CR>
        nnoremap <Leader>\ :NERDTreeToggle<CR>
        nnoremap <Leader>/ :NERDTreeFind<CR>
        nnoremap <Leader>f :Ag<CR>
        vnoremap <Leader>f :call <SID>FzfSelectedWord()<CR>
        nnoremap <Leader>F :FZF<CR>
        nnoremap <Leader>t :call <SID>FzfTagsCurrentWord()<CR>
        nnoremap <Leader>T :BTags<CR>
        nnoremap <Leader>b :Buffers<CR>
        nnoremap <Leader>B :bufdo bd<CR>
        nnoremap <Leader>m :Marks<CR>
        nnoremap <Leader>l :Lines<CR>
        nnoremap <Leader>L :BLines<CR>
        nnoremap <Leader>C :Commits<CR>
        nnoremap <Leader>g :GFiles<CR>
        nnoremap <Leader>G :GFiles?<CR>
        nnoremap <Leader>Gb :Gblame<CR>
        nnoremap <Leader>h :History<CR>
        nnoremap <Leader>q :set opfunc=<SID>SearchOperator<CR>g@
        vnoremap <Leader>q :<C-u>call <SID>SearchOperator(visualmode())<CR>
        nnoremap <Leader>Q :set opfunc=<SID>SearchProjectOperator<CR>g@
        vnoremap <Leader>Q :<C-u>call <SID>SearchProjectOperator(visualmode())<CR>
        nnoremap <Leader>r :split %:s?app/?spec/?:s?.rb?_spec.rb?<CR>
        nnoremap <Leader>R :split %:s?spec/?app/?:s?_spec.rb?.rb?<CR>
        nnoremap <Leader>S :Snippets<CR>
        nnoremap <Leader>o o<esc>
        nnoremap <Leader>O O<esc>
        nnoremap <Leader>c :call <SID>OpenInTerminal()<CR>
        nmap ]h <Plug>(GitGutterNextHunk)
        nmap [h <Plug>(GitGutterPrevHunk)
        vnoremap // y/<C-R>"<CR>
        vnoremap <Leader>a{ :Tabularize /^[^{]*/<CR>
        vnoremap <Leader>a= :Tabularize /^[^=]*/<CR>
        vnoremap <Leader>a: :Tabularize /:/<CR>
        vnoremap <Leader>A: :Tabularize /:\zs/<CR>
        cnoreabbrev W w
        cnoreabbrev Wq wq
        cnoreabbrev WQ wq
        cnoreabbrev Q q

        " options
        let g:fzf_tags_command = 'git ctags'
        let g:airline_theme = 'base16_flat'
        let g:airline#extensions#tabline#enabled = 1
        let g:airline_powerline_fonts = 1
        let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
        let g:nerdtree_tabs_autoclose = 0
        let NERDTreeAutoDeleteBuffer = 1
        let NERDTreeMinimalUI = 1
        let NERDTreeDirArrows = 1
        let NERDTreeWinSize = 32
        let g:UltiSnipsExpandTrigger="<tab>"
        let g:UltiSnipsJumpForwardTrigger="<c-j>"
        let g:UltiSnipsJumpBackwardTrigger="<c-k>"

        " search operator
        function! s:SearchOperator(type)
          if a:type ==# 'v'
            silent exec "normal! `<v`>\"ry"
          elseif a:type ==# 'char'
            silent exec "normal! `[v`]\"ry"
          else
            return
          endif
          let @/=@r
        endfunction

        function! s:SearchProjectOperator(type)
          if a:type ==# 'v'
            silent exec "normal! `<v`>\"ry"
          elseif a:type ==# 'char'
            silent exec "normal! `[v`]\"ry"
          else
            return
          endif
          silent exec "vimgrep! /" . shellescape(@@) . "/gj **/*.rb"
        endfunction

        " FZF-search visual-selected word
        function! g:GetVisualSelectionText()
          let [line_start, column_start] = getpos("'<")[1:2]
          let [line_end, column_end] = getpos("'>")[1:2]
          let lines = getline(line_start, line_end)
          if len(lines) == 0
            return ""
          endif
          let lines[-1] = lines[-1][: column_end - 2]
          let lines[0] = lines[0][column_start - 1:]
          return join(lines, "\n")
        endfunction

        function! s:FzfSelectedWord()
          let l:word = GetVisualSelectionText()
          call fzf#vim#ag(l:word, fzf#vim#with_preview())
        endfunction

        " FZF-search tags with word under a cursor
        function! s:FzfTagsCurrentWord()
          let l:word = expand('<cword>')
          let l:list = taglist(l:word)
          if len(l:list) == 1
            execute ':tag ' . l:word
          else
            call fzf#vim#tags(l:word)
          endif
        endfunction

        " open terminal with proper command
        function! s:OpenInTerminal()
          try
            silent exec "terminal ++close ++rows=8 ". b:termprg
          catch
            silent exec "terminal ++close ++rows=8"
          echo
          endtry
        endfunction

        " autocommands
        autocmd BufWritePre * :%s/\s\+$//e
        autocmd BufRead * normal zR
        autocmd FileType git nnoremap <C-]> ?^diff<CR>/ b<CR>3lv$h"fy:e <C-R>f<CR>
        autocmd FileType make setlocal noexpandtab
        autocmd FileType haskell setlocal makeprg=cabal\ build
        autocmd FileType haskell let b:termprg="ghci %:p"
        autocmd FileType nerdtree :vert resize 32
        autocmd FileType ruby let b:termprg="irb -r %:p"
        autocmd FileType ruby
          \ if expand("%") =~# '_spec\.rb$' |
          \   compiler rspec | setl makeprg=rspec\ --no-color\ % |
          \ else |
          \   setl makeprg=rubocop\ --format\ clang\ % |
          \ endif
        autocmd! FileType fzf set laststatus=0 noshowmode noruler | autocmd BufLeave <buffer> set laststatus=2 showmode ruler

        " commands
        command! MakeTags !git ctags
        command! Open !open %

        " colors
        hi SignColumn            ctermbg=0
        hi FoldColumn            ctermbg=0 ctermfg=7
        hi GitGutterAdd          ctermbg=0 ctermfg=2
        hi GitGutterChange       ctermbg=0 ctermfg=3
        hi GitGutterChangeDelete ctermbg=0 ctermfg=3
        hi GitGutterDelete       ctermbg=0 ctermfg=1
        hi DiffAdd               cterm=BOLD ctermfg=0 ctermbg=2
        hi DiffDelete            cterm=BOLD ctermfg=0 ctermbg=1
        hi DiffChange            cterm=BOLD ctermfg=0 ctermbg=3
        hi DiffText              cterm=BOLD ctermfg=0 ctermbg=2
        hi Directory             ctermfg=blue

        " FZF extension
        function! s:build_quickfix_list(lines)
          call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
          copen
          cc
        endfunction

        let g:fzf_action = { 'ctrl-q': function('s:build_quickfix_list'), 'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit' }
        let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
        let $FZF_DEFAULT_COMMAND = 'git ls-files'
      '';
      vimrcConfig.packages.myVimPackage = with pkgs.vimPlugins; {
        start = [
          fzfWrapper
          fzf-vim
          nerdtree
          vim-airline
          vim-airline-themes
          vim-gitgutter
          tabular
          syntastic
          YouCompleteMe
          ultisnips
        ];
      };
    })
    wget

    alacritty
    arandr
    chromium
    dunst
    feh
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    paper-icon-theme
    pinentry-qt
    qutebrowser
    rofi
    rofi-pass
    scrot
    slock
    xclip
    xdotool
    xmobar
    xmonad-with-packages
    xorg.xkill
    xorg.xmessage
    youtube-dl
  ];

  # users
  users.users = {
    placek = {
      uid = 1000;
      isNormalUser = true;
      description = "Paweł Placzyński";
      extraGroups = [ "wheel" "networkmanager" "docker" ];
      packages = with pkgs; [
        # arduino
        # blender
        # eagle
        # gimp
        # inkscape
        # libreoffice-fresh
        # mplayer
        # musescore
        # shotwell
        # virtualbox
        # vnstat
      ];
      shell = pkgs.fish;
    };
  };

  services = {
    acpid.enable = true;
    cron.enable = true;
    greenclip.enable = true;
    printing.enable = true;
    xserver = {
      displayManager.defaultSession = "none+xmonad";
      displayManager.lightdm = {
        enable = true;
        greeters.mini.enable = true;
        greeters.mini.user = "placek";
        greeters.mini.extraConfig = ''
          [greeter]
          show-password-label = false
          invalid-password-text = nope!
          show-input-cursor = false
          password-alignment = left
          password-input-width = 12
          [greeter-theme]
          font = "Iosevka"
          font-weight = normal
          error-color = "#F5F5F5"
          password-color = "#F5F5F5"
          background-color = "#5F5F5F"
          window-color = "#2C3E50"
          border-color = "#3498DB"
          border-width = 4px
          password-background-color = "#2C3E50"
          password-border-width = 0px
        '';
      };
      enable = true;
      layout = "pl";
      libinput.enable = true;
      windowManager.xmonad = {
        enableContribAndExtras = true;
        haskellPackages = pkgs.haskell.packages.ghc865;
        extraPackages = haskellPackages: with haskellPackages; [
          alsa-core
          alsa-mixer
          xmonad
          xmonad-contrib
          xmonad-extras
        ];
        enable = true;
      };
      xautolock = {
        enable = true;
        enableNotifier = true;
        locker = "${pkgs.slock}/bin/slock";
        notifier = "${pkgs.libnotify}/bin/notify-send 'Locking in 10 seconds'";
      };
    };
  };

  # systemd.user.services.dotfiles = {
  #   description = "Dot files synchronization";
  #   enable = true;
  #   serviceConfig = {
  #     Environment = [
  #       "DOTFILES_URL=git@github.com:placek/dotfiles.git"
  #       "DOTFILES_DIR=.config/dotfiles"
  #     ];
  #     Type             = "oneshot";
  #     WorkingDirectory = "/home/placek/.config/dotfiles";
  #     RemainAfterExit  = "yes";
  #     ExecStartPre     = "-${pkgs.git}/bin/git clone --recurse-submodules $DOTFILES_URL $DOTFILES_DIR";
  #     ExecStart        = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --stow";
  #     ExecReloadPre    = "-${pkgs.git}/bin/git pull ";
  #     ExecReload       = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --restow";
  #     ExecStop         = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --delete";
  #   };
  #   wantedBy = [ "default.target" ];
  # };
}
