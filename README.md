https://github.com/MiniApollo/kickstart.emacs/assets/72389030/5c66130d-66b9-459b-a26d-210f3f937459

# Table of Contents

1.  [Introduction](#orgb229cbd)
    -  [Packages](#orgb05d649)
    -  [Helpful resources](#orgfaf0570)
2.  [Installation](#orgb633c86)
    -  [1. Requirements](#orgb7bc22f)
    -  [2. Backup your previous configuration](#org6189661)
    -  [3. Clone the repository to the configuration location](#org820a205)
    -  [4. Start Emacs](#orgd77a070)
3.  [Post Installation](#org60302a9)
    -  [1. Install fonts](#org87d8fc9)
    -  [2. Open the configuration file](#org94fe140)
    -  [3. Fork the repository](#org23b14b0)
4.  [Uninstallation](#org14852f4)
5.  [Gallery](#orgc18728a)

<a id="orgb229cbd"></a>

# Introduction
This repository gives you a starting point for Gnu Emacs with good defaults, optional vim keybindings and packages that most people may want to use.

Kickstart.emacs is **not** a distribution. <br>
It's a template for your own configuration.

This config is:
-   A single file **org document** (with examples of moving to multi-file)
-   Modular and easily configurable
-   Documented describing its purpuse

Inspired by [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)

Special thanks to:
-   [DistroTube](https://www.youtube.com/watch?v=d1fgypEiQkE&list=PL5--8gKSku15e8lXf7aLICFmAHQVo0KXX)
-   [System Crafters](https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ)

Their content helped me to create this configuration.

<a id="orgb05d649"></a>

## Packages

### Included Package list

-   Package Manager: Package.el with Use-package (built in)
-   Optin [Evil mode](https://github.com/emacs-evil/evil): An extensible vi/vim layer
-   [General](https://github.com/noctuid/general.el): Keybindings
-   [Gruvbox-theme](https://github.com/greduan/emacs-theme-gruvbox): Color scheme
-   [Doom-modeline](https://github.com/seagle0128/doom-modeline): Prettier, more useful modeline
-   [Nerd Icons](https://github.com/rainstormstudio/nerd-icons.el): For icons and more helpful ui (Supports both GUI and TUI)
-   [Projectile](https://github.com/bbatsov/projectile): Project interaction library
-   [Eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html): Language Server Protocol Support
-   [Sideline-Flymake](https://github.com/emacs-sideline/sideline-flymake): Show flymake errors with sideline 
-   [Yasnippet](https://github.com/joaotavora/yasnippet): Template system and snippet collection package
-   Optin [Tree-Sitter](https://tree-sitter.github.io/tree-sitter): A parser generator tool and an incremental parsing library.
-   Some [Org mode](https://orgmode.org/) packages (toc-org, org-superstar)
-   [Eat](https://codeberg.org/akib/emacs-eat): Fast terminal emulator within Emacs
-   [Magit](https://github.com/magit/magit): Complete text-based user interface to Git
-   [Diff-hl](https://github.com/dgutov/diff-hl): Highlights uncommitted changes
-   [Corfu](https://github.com/minad/corfu): Enhances in-buffer completion
-   [Cape](https://github.com/minad/cape): Provides Completion At Point Extensions
-   [Orderless](https://github.com/oantolin/orderless): Completion style that matches candidates in any order
-   [Vertico](https://github.com/minad/vertico): Provides a performant and minimalistic vertical completion UI.
-   [Marginalia](https://github.com/minad/marginalia): Adds extra metadata for completions in the margins (like descriptions).
-   [Consult](https://github.com/minad/consult): Provides search and navigation commands.
-   [Helpful](https://github.com/Wilfred/helpful): A better Emacs *help* buffer 
-   [Diminish](https://github.com/myrjola/diminish.el): Hiding or abbreviation of the modeline displays
-   [Rainbow Delimiters](https://github.com/Fanael/rainbow-delimiters): Adds colors to brackets.
-   [Which key](https://github.com/justbur/emacs-which-key): Helper utility for keychords
-   [Ws-butler](https://github.com/lewang/ws-butler): Removes whitespace from the ends of lines.

### Recommended Packages

If you want to see how to configure these, look up their git repositories or check out my [config](https://github.com/MiniApollo/config/blob/main/emacs/config.org).

-   **[DashBoard](https://github.com/emacs-dashboard/emacs-dashboard):** Extensible startup screen.
-   **[Drag Stuff](https://github.com/rejeep/drag-stuff.el):** Makes it possible to move selected text, regions and lines.
-   **[Rainbow Mode](https://github.com/emacsmirror/rainbow-mode):** Displays the actual color as a background for any hex color value (ex. #ffffff).
-   **[UndoTree](https://www.emacswiki.org/emacs/UndoTree):** Visualizes the undo history (alternative: [Vundo](https://github.com/casouri/vundo) with [undo-fu-session](https://github.com/emacsmirror/undo-fu-session)).
-   **[Vterm](https://github.com/akermu/emacs-libvterm):** Fast, Fully-fledged terminal emulator inside GNU Emacs.
-   **[Multi-vterm](https://github.com/suonlight/multi-vterm):** Managing multiple vterm buffers in Emacs 
-   **[Sudo-edit](https://github.com/nflath/sudo-edit):** Utilities for opening files with root privileges (also works with doas).

<a id="orgfaf0570"></a>

## Helpful resources

Videos and configurations to get started.

-   **[Emacs From Scratch by System Crafters](https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ):** Very detailed video series about building Gnu Emacs from the ground up.
-   **[Configuring Emacs by DistroTube](https://www.youtube.com/watch?v=d1fgypEiQkE&list=PL5--8gKSku15e8lXf7aLICFmAHQVo0KXX):** Shorter but also very detailed video series about configuring Emacs from scratch.
-   **[Mastering Emacs](https://www.masteringemacs.org/):** An awesome blog covering interesting topics and practical tips about Emacs.
-   **[Purcell's emacs configuration](https://github.com/purcell/emacs.d):** Emacs configuration bundle with batteries included.
-   **[Spacemacs](https://www.spacemacs.org/) and [Doom Emacs](https://github.com/doomemacs/doomemacs):** For an out of box experience and their wiki pages are really helpful.
-   **[More starter kits](https://www.emacswiki.org/emacs/StarterKits ):** List of starter kits for Emacs.
-   **[Emacs manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html):** For learning the fundamentals of Emacs.


<a id="orgb633c86"></a>

# Installation


<a id="orgb7bc22f"></a>

## 1. Requirements

-   Gnu Emacs 30.1 or later (Latest stable release)
-   Git (To clone/download this repository)


### Optional:

-   ripgrep
-   fd (improves file indexing performance for some commands)
-   Gnu Emacs with [native-compilation](https://www.emacswiki.org/emacs/GccEmacs) (provides noticeable performance improvements)


<a id="org6189661"></a>

## 2. Backup your previous configuration

If any exists.

<a id="org820a205"></a>

## 3. Clone the repository to the configuration location

### Linux and Mac
```sh
git clone https://github.com/MiniApollo/kickstart.emacs.git "${XDG_CONFIG_HOME:-$HOME/.config}"/emacs
```

### Windows

-   **CMD:**
```sh
git clone https://github.com/MiniApollo/kickstart.emacs.git %userprofile%\AppData\Local\emacs\
```
-   **Powershell:**
```sh
git clone https://github.com/MiniApollo/kickstart.emacs.git $env:USERPROFILE\AppData\Local\emacs\
```

<a id="orgd77a070"></a>

## 4. Start Emacs

Emacs will install all the requested packages (it can take a minute).

> **Note:**
> If you see errors, warnings when package installation is finished just restart Emacs.

<a id="org60302a9"></a>

# Post Installation

<a id="org87d8fc9"></a>

## 1. Install fonts

Run the following command with M-x (alt-x) C-y to paste

```sh
nerd-icons-install-fonts
```

Change or install JetBrains Mono font

<a id="org94fe140"></a>

## 2. Open the configuration file

1.  Hit Ctrl-Space-s-c to open the config file at $HOME/.config/emacs

> **Note**
> If you use Windows you need to change the path (hit C-x C-f, find the config file and in general region replace the path)

2.  Now you can Edit and add more configuration.

<a id="org23b14b0"></a>

## 3. Fork the repository

Recommended so that you have your own copy to modify.

<a id="org14852f4"></a>

# Uninstallation

To uninstall kickstart.emacs, you need to remove the following directory:

-   Delete the emacs folder/directory for your OS (E.g. $HOME/.config/emacs/).

<a id="orgc18728a"></a>

# Gallery

![Emacs_KickStarter](https://github.com/MiniApollo/kickstart.emacs/assets/72389030/b82bb86b-ce49-4b0a-8fe7-2ca8b8c422fb)
![Kickstart_coding](https://github.com/MiniApollo/kickstart.emacs/assets/72389030/8e560d2b-78f5-4306-8f6a-c70ad189f181)
