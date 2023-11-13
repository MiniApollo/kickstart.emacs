;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(require 'use-package) ;; requires package.el and use-package so we can use it
(setq use-package-always-ensure t) ;; always ensures that a package is installed
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq evil-want-keybinding nil) ;; Disable evil bindings in other modes (It's not consistent and not good)
  (setq evil-want-C-u-scroll t) ;; Set  C-u to scrool up
  (setq evil-want-C-i-jump nil) ;; Disables C-i jump
  (setq evil-undo-system 'undo-redo) ;; C-r to redo
  (setq org-return-follows-link  t) ;; Sets RETURN key in org-mode to follow links
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  ;; Setting where to use evil-collection
  (setq evil-collection-mode-list '(dired ibuffer magit))
  (evil-collection-init))
;; Unmap keys in 'evil-maps. If not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package general
  :config
  (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer start/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "C-SPC") ;; access leader in insert mode

  (start/leader-keys
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "p" '(projectile-command-map :wk "Projectile command map"))

  (start/leader-keys
    "f" '(:ignore t :wk "Find")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
    "f r" '(counsel-recentf :wk "Recent files"))

  (start/leader-keys
    "b" '(:ignore t :wk "Buffer Bookmarks")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b j" '(bookmark-jump :wk "Bookmark jump"))

  (start/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d v" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current"))

  (start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g g" '(magit-status :wk "Magit status"))

  (start/leader-keys
    "h" '(:ignore t :wk "Help")
    "h c" '(company-manual-begin :wk "Company completion at point")
    "h r" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el"))
            :wk "Reload emacs config"))

  (start/leader-keys
    "s" '(:ignore t :wk "Show")
    "s e" '(eat :wk "Show Eat"))

  (start/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")))

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)       ;; Turns on automatic parens pairing

(global-display-line-numbers-mode 1) ;; Display line numbers
(global-visual-line-mode t)  ;; Enable truncated lines
(menu-bar-mode -1)           ;; Disable the menu bar
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar

(setq mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
(setq scroll-conservatively 10) ;; Smooth scrolling when going down with scroll margin
(setq scroll-margin 8)

(global-set-key [escape] 'keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
(setq make-backup-files nil) ; Stop creating ~ backup files
(blink-cursor-mode 0) ;; Don't blink cursor
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally

(setq org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
(setq-default tab-width 4)

(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-medium t)) ;; We need to add t to trust this package

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(set-face-attribute 'default nil
                    ;; :font "JetBrains Mono" ;; Set your favorite type of font or download JetBrains Mono
                    :height 120
                    :weight 'medium)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package projectile
  :config
  (projectile-mode 1)
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))) ;; . 1 means only search first subdirectory level for projects
;; Use Bookmarks for non git projects

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; To Disable features https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  :hook (;; Automatic Language Modes
         (prog-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package lua-mode
  :mode "\\.lua\\'")

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(with-eval-after-load 'org
  (require 'org-tempo))

(use-package eat
  :hook('eshell-load-hook #'eat-eshell-mode))

(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit
  :commands magit-status)

(use-package company
  :defer 2
  :diminish
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)) ;; You can delete the :bind region to use return (default)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after compan
  :diminish
  :hook (company-mode . company-box-mode))

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package nerd-icons-ivy-rich
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package diminish)

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → " ))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
