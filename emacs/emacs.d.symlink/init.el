;; Package Config
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)

;; Bootstrap `use-package'
; From: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package evil
	     :ensure t
	     :config (evil-mode 1))

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Appearance
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(set-face-attribute 'default nil :family "Source Code Pro" :height 100)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-eighties-dark t)
