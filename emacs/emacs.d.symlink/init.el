(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(add-to-list 'load-path (expand-file-name "site-packages/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "package-config/" user-emacs-directory))
(add-to-list 'load-path "~/dev/elisp/org-research/")

;; Frame Configuration
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 90))

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
  (package-install 'use-package))
(require 'use-package)

(require 'server)
(unless (server-running-p)
  (server-mode 1))

;; Load evil-mode and swiper/ivy early on
(require 'init-evil-mode)
(require 'init-swiper)
(require 'init-which-key)

;;------------------------------------------------------------------------------
;;                            Editor Settings
;;------------------------------------------------------------------------------

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'css-indent-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

;; Editing
(blink-cursor-mode 0)
(setq-default fill-column 80)
(setq case-fold-search t)
(setq require-final-newline t)

(use-package whitespace
  :ensure t
  :defer t
  :init (progn
          (add-hook 'prog-mode-hook (lambda ()
                                      (setq show-trailing-whitespace t)))
          (add-hook 'before-save-hook 'whitespace-cleanup)))

(defun alex/toggle-reading-mode ()
  "Centres the current buffer's text according to the fill-column width."
  (interactive)
  (let ((left-margin (car (window-margins)))
        (right-margin (cdr (window-margins))))
    (if (or (eq left-margin nil) (eq right-margin nil))
        (let ((new-width (/ (- (window-body-width) fill-column) 2)))
          (setq left-margin-width new-width)
          (setq right-margin-width new-width)
          (set-window-margins nil new-width new-width))
      (progn
        (setq left-margin-width nil)
        (setq right-margin-width nil)
        (set-window-margins nil nil nil)))))

;; Backups
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package paren
  :ensure t
  :init (progn
          (setq show-paren-delay 0
                show-paren-style 'parenthesis)
          (add-hook 'prog-mode-hook 'show-paren-mode)))

(use-package doc-view
  :ensure t
  :init (progn
          (add-hook 'doc-view-mode-hook 'auto-revert-mode)))

;; ------------------------------------------------------------------------------
;;                             Custom Functions
;; ------------------------------------------------------------------------------

(defun alex/delete-file-and-buffer ()
  "Removes the file connected to the current buffer and kills the
buffer. (From Spacemacs)"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-this-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file?")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun alex/rename-current-buffer-file ()
  "Renames the current buffer's file"
  (interactive)
  (let ((filename (buffer-file-name))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format ("Create directory '%s'?" dir)))
                            (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (message "File '%s' renamed to '%s'" name (file-name-nondirectory new-name)))))))))

(defun alex/show-buffer-name ()
  "Print the current file's path in the minibuffer"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (message filename)
      (error "Buffer not visiting a file"))))

(defun alex/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; ------------------------------------------------------------------------------
;;                             Keybindings
;; ------------------------------------------------------------------------------

;; General/Random Keybindings
(evil-leader/set-key
  "~" 'ansi-term
  "`" 'mu4e)

;; Buffer Management Key Bindings
(evil-leader/set-key
  "b b" 'switch-to-buffer
  "b d" 'kill-this-buffer
  "b k" 'kill-buffer
  "b l" 'list-buffers
  "b K K" 'desktop-clear)

;; File Management Key Bindings
(evil-leader/set-key
  "f f" 'counsel-find-file
  "f d" 'dired
  "f D" 'alex/delete-file-and-buffer
  "f R" 'alex/rename-current-buffer-file
  "f s" 'evil-write
  "f S" 'evil-write-all
  "f y" 'alex/show-buffer-name)

;; Window Management Key Bindings
(evil-leader/set-key
  "w =" 'balance-windows
  "w c" 'delete-window
  "w C" 'delete-other-windows
  "w h" 'evil-window-left
  "w H" 'evil-window-move-far-left
  "w j" 'evil-window-down
  "w J" 'evil-window-move-very-bottom
  "w k" 'evil-window-up
  "w K" 'evil-window-move-very-top
  "w l" 'evil-window-right
  "w L" 'evil-window-move-far-right
  "w w" 'other-window
  "w s" 'split-window-below
  "w v" 'split-window-right)

(which-key-add-key-based-replacements
  "SPC w" "Windows"
  "SPC f" "Files"
  "SPC x" "Text"
  "SPC m" "Mode"
  "SPC b" "Buffer"
  "SPC h" "Help")

;; Help Keybindings
(evil-leader/set-key
  "h f" 'counsel-describe-function
  "h m" 'describe-mode
  "h v" 'counsel-describe-variable
  "h b" 'describe-bindings
  "h p" 'describe-package
  "h i" 'info
  "h M" 'man)

;; Text Editing Keybindings
(evil-leader/set-key
  "x u"   'downcase-region
  "x U"   'upcase-region
  "x a r" 'align-regexp
  "x d w" 'delete-trailing-whitespace
  "x i r" 'indent-region
  "x i b" 'alex/indent-buffer
  "x C" 'alex/toggle-reading-mode)

;; Elisp Editing Bindings
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "m e b" 'eval-buffer
  "m e r" 'eval-region
  "m e e" 'eval-last-sexp)

;; In emacs-mac-port, make the ALT key META and the CMD key
;; SUPER. Also free up the right ALT key for inputting special
;; symbols. Oh, and add a couple of default OS X key bindings to the
;; super key.
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil)
  (setq mac-command-modifier 'super)
  (setq mac-pass-command-to-system t)
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super w)] 'delete-frame)
  (global-set-key [(super n)] 'make-frame)
  (global-set-key [(super z)] 'undo)
  (global-set-key [(super q)] nil)
  (global-set-key [(super ctrl f)] 'toggle-frame-fullscreen))

;; Appearance
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode 0)
(when (display-graphic-p)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

(let ((font-name "Consolas"))
  (when (member font-name (font-family-list))
    (set-face-attribute 'default nil :font font-name :height 110)))

(use-package leuven-theme
  :ensure t
  :init (progn (load-theme 'leuven t)))


;; Match fringe colour to background colour
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(global-hl-line-mode 1)

;;------------------------------------------------------------------------------
;;                  Load Packages that need configuring
;;                  (i.e., everything in use-packages/)
;;------------------------------------------------------------------------------

(require 'init-exec-path-from-shell)
(require 'init-paredit)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-rainbow-delimiters)
(require 'init-magit)
(require 'init-flyspell)
(require 'init-paradox)
(require 'init-ag)
(require 'init-diff-hl)

(require 'init-calendar)
(require 'init-org)

(require 'init-ledger-mode)
(require 'init-matlab-mode)
(require 'init-markdown-mode)
(require 'init-scss-mode)
(require 'init-web-mode)
(require 'init-company)
(require 'init-latex)
(require 'init-golang)
(require 'init-haskell)

(require 'init-mu4e)

;;------------------------------------------------------------------------------
;;           Load packages that don't need tonnes of configuration
;;------------------------------------------------------------------------------

; Use gpg2 for compatibility with Yubikey 4
(use-package epa :ensure t :config (progn (setq epg-gpg-program "gpg2")))
(use-package osx-pseudo-daemon :ensure t :if (memq window-system '(mac ns)))
(use-package yaml-mode :ensure t)
(use-package fish-mode :ensure t :mode ("\\.fish\\'" . fish-mode))
(use-package julia-mode :ensure t :mode ("\\.jl\\'" . julia-mode))
