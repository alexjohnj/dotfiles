(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/site-packages/")
;; Package Config
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Editing
(setq fill-column 79)
(setq case-fold-search t)
(setq require-final-newline t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Appearance
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-linum-mode t)

(set-face-attribute 'default nil :family "Source Code Pro" :height 100)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-eighties-dark t)
;; Match fringe colour to background colour
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; Configure/Install Packages
;; Bootstrap `use-package'
;; From: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
 default (package-install 'use-package))
(require 'use-package)

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package powerline
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (progn
        (load-library "powerline-srgb-offset")
        (powerline-srgb-offset-add-theme "base16-eighties"
                                         '('("mode-line" "#646464")
                                           '("powerline-active1" "#484848")
                                           '("powerline-active2" "#797979")
                                           '("mode-line-inactive" "#494949")
                                           '("powerline-inactive1" "#242424")
                                           '("powerline-inactive2" "#424242")))
        (powerline-srgb-offset-activate "base16-eighties")))
  (powerline-default-theme))

(use-package ledger-mode
  :ensure t
  :config (if (eq system-type 'darwin) (setq ledger-binary-path "/usr/local/bin/ledger")))

(use-package highlight-current-line
  :ensure t
  :config
  (global-hl-line-mode t)
  (setq highlight-current-line-globally t)
  (setq highlight-current-line-high-faces nil)
  (setq highlight-current-line-whole-line nil)
  (setq hl-line-face (quote highlight)))

(use-package matlab-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode)))
