(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(add-to-list 'load-path (expand-file-name "site-packages/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "package-config/" user-emacs-directory))

;;------------------------------------------------------------------------------
;;                         Package Configuration
;;------------------------------------------------------------------------------
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)
;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
 default (package-install 'use-package))
(require 'use-package)

;;------------------------------------------------------------------------------
;;                            Editor Settings
;;------------------------------------------------------------------------------

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
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-linum-mode t)

(set-face-attribute 'default nil :family "Source Code Pro" :height 100)

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
(load-theme 'base16-eighties-dark t)
;; Match fringe colour to background colour
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;;------------------------------------------------------------------------------
;;                  Load Packages that need configuring 
;;                  (i.e., everything in use-packages/)
;;------------------------------------------------------------------------------

(require 'init-evil-mode)
(require 'init-exec-path-from-shell)
(require 'init-powerline)
(require 'init-highlight-current-line)
(require 'init-paredit)
(require 'init-ledger-mode)
(require 'init-matlab-mode)

;;------------------------------------------------------------------------------
;;               Load Packages that DON'T need configuring
;;------------------------------------------------------------------------------

(use-package clojure-mode :ensure t)
(use-package clojure-mode-extra-font-locking :ensure t)
(use-package cider :ensure t)
