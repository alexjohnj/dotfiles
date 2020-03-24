;;; Basic Configuration

;; The following performance tweaks are taken from Doom Emacs.
;; Increase garbage collection threshold during init to improve performance.
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Temporarily disable the file handler list during startup for improved performance.
(defvar alex--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore default settings after Emacs has started up.
(add-hook 'emacs-startup-hook '(lambda ()
                                 (setq gc-cons-threshold 16777216
                                       gc-cons-percentage 0.1
                                       file-name-handler-alist alex--file-name-handler-alist)))
;; Stop Emacs from dumping customise values in init file.
(let ((custom (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p custom)
    (with-temp-buffer
      (write-file custom)))
  (setq custom-file custom))

(load custom-file)

;; Setup the load path
(add-to-list 'load-path (expand-file-name "site-packages/" user-emacs-directory)) ; Non-ELPA packages
(add-to-list 'load-path (expand-file-name "package-config/" user-emacs-directory)) ; Package configuration

;; Configure a default frame size
(add-to-list 'default-frame-alist (cons 'height (display-pixel-height)))
(add-to-list 'default-frame-alist '(width . 120))

;; Say hi
(setq user-full-name "Alex Jackson"
      user-mail-address "alex@alexj.org")


;;; Package Manager Configuration
(require 'package)

;; Set package sources as MELPA-stable, MELPA, org and GNU. If GnuTLS is
;; available, use HTTPS versions of these sources. Otherwise, fallback to HTTP
;; versions. I sometimes run Emacs on Windows, so this check is necessary
;; otherwise package management will be broken.
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos)) (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (setq package-archives `(("melpa-stable" . ,(concat proto "://stable.melpa.org/packages/"))
                           ("melpa" . ,(concat proto "://melpa.org/packages/"))
                           ("org" . ,(concat proto "://orgmode.org/elpa/"))
                           ("gnu" . ,(concat proto "://elpa.gnu.org/packages/")))))

;; Prefer stable versions of packages over snapshots. I've had essential tools
;; like magit break too many times using snapshots.
(setq package-archive-priorities '(("melpa-stable" . 20)
                                   ("org" . 15)
                                   ("gnu" . 10)
                                   ("melpa" . 5)))

;; Pin use-package to a snapshot build because the last stable version is from
;; 2016 but I switched to using stable packages in 2018. As a result, much of my
;; configuration is written for newer versions of use-package.
(setq package-pinned-packages '((use-package . "melpa")))

(package-initialize)

;; Bootstrap use-package on new systems. use-package will be used from now on
;; for package management.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;; Emacs Server/Daemon

;; Start a new Emacs server instance if one isn't running.
(require 'server)
(unless (server-running-p)
  (server-mode 1))


;;; Core Packages

;; I've set up the configuration for core packages early in init because they're used so much later on.

;; Ensure the system PATH is picked up properly by Emacs
(use-package exec-path-from-shell
  :unless (memq system-type '(windows-nt ms-dos))
  :config (progn
            (add-to-list 'exec-path-from-shell-variables "GPG_AGENT_INFO")
            (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
            (add-to-list 'exec-path-from-shell-variables "SSH_AGENT_PID")
            (exec-path-from-shell-initialize)))

;; Keep the modeline neat and tidy
(use-package diminish
  :commands diminish)

;; Load evil and associated packages. Note evil-leader must be loaded BEFORE
;; evil.
(use-package evil-leader
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (global-evil-leader-mode)))

(use-package evil
  :after evil-leader
  :diminish undo-tree-mode
  :config (progn (evil-mode 1)))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :bind (("S-/" . evil-commentary))
  :init (progn (evil-commentary-mode)))

(use-package evil-surround
  :after evil
  :init (global-evil-surround-mode 1))

(use-package evil-escape
  :after evil
  :init (evil-escape-mode)
  :diminish evil-escape-mode
  :config (progn
            (setq-default evil-escape-key-sequence "fd"
                          evil-escape-delay 0.1)))

(use-package ivy
  :diminish ivy
  :hook (after-init . ivy-mode)
  :config (progn
            (evil-leader/set-key "s" 'ivy-resume)
            (setq ivy-use-virtual-buffers t
                  ivy-count-format "(%d/%d)"
                  ivy-initial-inputs-alist nil)
            (with-eval-after-load "ivy"
              (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-dispatching-done)
              (define-key ivy-minibuffer-map (kbd "<C-M-return>") 'ivy-immediate-done)
              (define-key ivy-minibuffer-map [escape] (kbd "C-g")))))

(use-package swiper
  :after ivy
  :diminish ivy-mode)

(use-package counsel
  :commands counsel-imenu
  :bind (("C-s" . 'counsel-grep-or-swiper)
         ("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file))
  :init (evil-leader/set-key "i" 'counsel-imenu)
  :config (progn
            (when (executable-find "rg")
              (setq counsel-grep-base-command
                    "rg -i --no-heading --line-number --color never '%s' %s"))))

(use-package which-key
  :diminish which-key-mode
  :init (progn
          (setq which-key-idle-delay 0.3)
          (which-key-setup-side-window-right-bottom)
          (which-key-mode)))


;;; Editor Settings

;; Set the correct indentation level and character.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'css-indent-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

(blink-cursor-mode 0) ; Disable blinking cursor

(global-auto-revert-mode 1)
(setq-default fill-column 80)
(setq require-final-newline t) ; Insert newline at end of file when saved.
(setq case-fold-search t) ; Makes searches case-insensitive
(fset 'yes-or-no-p 'y-or-n-p) ; Change yes-no prompts to y-n

(add-hook 'prog-mode-hook
          '(lambda () (setq show-trailing-whitespace t)))

;; Enable emoji, and stop the UI from freezing when trying to display them on a Mac.
(when (and (eq system-type 'darwin)
           (fboundp 'set-fontset-font))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; Hide window accessories
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; Hide the menu bar on everything except macOS.
(if (and (eq system-type 'darwin) (display-graphic-p))
    (menu-bar-mode +1)
  (menu-bar-mode -1))

;; Editing contextual info
(global-hl-line-mode 1)
(line-number-mode)
(column-number-mode)

;; Save backups to a temporary directory instead of the current directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Trim tidy whitespace before saving
(use-package whitespace
  :hook (before-save . whitespace-cleanup))

;; Highlight matching parentheses
(use-package paren
  :init (progn
          (setq show-paren-delay 0
                show-paren-style 'parenthesis))
  :hook (prog-mode . show-paren-mode))

;; Pretty print line-feed characters. I think the only place I use line-feeds is
;; in this file!
(use-package page-break-lines
  :hook (emacs-lisp-mode . page-break-lines-mode))

;; Unfill text
(use-package unfill
  :bind (("C-M-q" . unfill-paragraph)))

;; Aggressively indent code in certain modes
(use-package aggressive-indent
  :pin melpa
  :hook (emacs-lisp-mode . aggressive-indent-mode))


;;; Appearance

;; Disable the noisy startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Flash the mode line instead of playing the bell sound.
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Use my custom font if it's installed.
(let ((font-name "JetBrains Mono"))
  (when (member font-name (font-family-list))
    (if (memq window-system '(mac ns)) ; Font scaling is a bit different between
                                        ; macOS and other platforms.
        (set-face-attribute 'default nil :font font-name :height 130)
      (set-face-attribute 'default nil :font font-name :height 110))))

(use-package solarized-theme
  :pin melpa
  :config (progn
            (setq solarized-use-variable-pitch nil
                  solarized-scale-org-headlines nil
                  solarized-scale-outline-headlines nil)
            (load-theme 'solarized-light t)))

;; Match the fringe colour to the theme's background colour
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))


;;; Editing Functions

;; Wrap and centre the buffer's text to make reading on a widescreen monitor a bit more pleasant.
(defun alex/toggle-reading-mode ()
  "Centres the current buffer's text according to the 'fill-column' width + a small buffer region."
  (interactive)
  (let ((left-margin (car (window-margins)))
        (right-margin (cdr (window-margins))))
    (if (or (eq left-margin nil) (eq right-margin nil))
        (let ((new-width (- (/ (- (window-body-width) fill-column) 2) 30)))
          (setq left-margin-width new-width)
          (setq right-margin-width new-width)
          (set-window-margins nil new-width new-width))
      (progn
        (setq left-margin-width nil)
        (setq right-margin-width nil)
        (set-window-margins nil nil nil)))))

;; Indent the contents of the current buffer.
(defun alex/indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


;;; Buffer Management

;; Here I've defined some custom functions for buffer management. I ripped them
;; from Spacemacs a few years ago. They're bound in the "Keybindings" section.

(defun alex/delete-file-and-buffer ()
  "Remove the file connected to the current buffer and kill the buffer (from Spacemacs)."
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
  "Renames the current buffer's file."
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
  "Print the current file's path in the minibuffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (message filename)
      (error "Buffer not visiting a file"))))


;;; Basic Keybindings

;; I define keybindings for general editor functionality in this section. Mode
;; specific keybindings are defined in their use-package configurations. I use
;; evil-leader extensively with different categories of keybindings prefixed by
;; a common key.

;; Little helper to avoid repeating the prefix for definitions.
(defun alex/evil-leader--prefix (prefix key def &rest bindings)
  "Define evil-leader keys under PREFIX. So the shortcut becomes LEADER PREFIX KEY to execute DEF."
  (while key
    (evil-leader/set-key (concat prefix key) def)
    (setq key (pop bindings)
          def (pop bindings))))

;; General bindings. These don't have a common prefix.
(evil-leader/set-key
  "~" 'ansi-term
  "`" 'mu4e)

;; Buffer Management.
(alex/evil-leader--prefix "b"
                          "b" 'switch-to-buffer
                          "d" 'kill-this-buffer
                          "k" 'kill-buffer
                          "l" 'list-buffers
                          "K K" 'desktop-clear)

;; File Management
(alex/evil-leader--prefix "f"
                          "f" 'counsel-find-file
                          "d" 'dired
                          "D" 'alex/delete-file-and-buffer
                          "R" 'alex/rename-current-buffer-file
                          "s" 'evil-write
                          "S" 'evil-write-all
                          "y" 'alex/show-buffer-name)

;; Window Management
(alex/evil-leader--prefix "w"
                          "=" 'balance-windows
                          "c" 'delete-window
                          "C" 'delete-other-windows
                          "h" 'evil-window-left
                          "H" 'evil-window-move-far-left
                          "j" 'evil-window-down
                          "J" 'evil-window-move-very-bottom
                          "k" 'evil-window-up
                          "K" 'evil-window-move-very-top
                          "l" 'evil-window-right
                          "L" 'evil-window-move-far-right
                          "s" 'split-window-below
                          "v" 'split-window-right)

(use-package ace-window
  :commands ace-window
  :init (alex/evil-leader--prefix "w"
                                  "w" 'ace-window
                                  "d" 'ace-delete-window
                                  "S" 'ace-swap-window))

;; Help System
(alex/evil-leader--prefix "h"
                          "f" 'counsel-describe-function
                          "m" 'describe-mode
                          "v" 'counsel-describe-variable
                          "b" 'describe-bindings
                          "p" 'describe-package
                          "i" 'info
                          "M" 'man)

(alex/evil-leader--prefix "h a"
                          "a" 'apropos
                          "c" 'apropos-command
                          "d" 'apropos-documentation
                          "v" 'apropos-variable
                          "o" 'apropos-user-option)

;; Text Editing
(alex/evil-leader--prefix "x"
                          "u"   'downcase-region
                          "U"   'upcase-region
                          "a r" 'align-regexp
                          "d w" 'delete-trailing-whitespace
                          "i r" 'indent-region
                          "i b" 'alex/indent-buffer
                          "C" 'alex/toggle-reading-mode)

;; Elisp Editing
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "m e b" 'eval-buffer
  "m e r" 'eval-region
  "m e e" 'eval-last-sexp)

;; Set up which-key hints for leader prefixes.
(which-key-add-key-based-replacements
  "SPC w" "Windows"
  "SPC f" "Files"
  "SPC x" "Text"
  "SPC m" "Mode"
  "SPC b" "Buffer"
  "SPC h" "Help")

;; In emacs-mac-port, make the ALT key META and the CMD key SUPER. Also free up
;; the right ALT key for inputting special symbols. Oh, and add a couple of
;; default OS X key bindings to the super key.
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


;;; Core Packages

(use-package smartparens
  :pin melpa
  :diminish
  :init (progn
          (require 'smartparens-config)
          (smartparens-global-mode t)))

(require 'init-projectile)
(require 'init-flycheck)
(require 'init-rainbow-delimiters)
(require 'init-magit)
(require 'init-flyspell)
(require 'init-paradox)
(require 'init-ag)
(require 'init-deft)
(require 'init-ediff)

(use-package epa
  :config (progn
            (setq epg-gpg-program "gpg2"))) ; Use gpg2 for Yubikey 4 compatibility


;;; Language Packages

(require 'init-lsp)
(require 'init-ledger-mode)
(require 'init-beancount-mode)
(require 'init-markdown-mode)
(require 'init-scss-mode)
(require 'init-web-mode)
(require 'init-company)
(require 'init-latex)
(require 'init-swift)
(require 'init-ruby)
(require 'init-elixir)

(use-package json-mode
  :pin melpa
  :mode ("\\.json\\'" . json-mode))

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

;; For the rare time I have to view PHP code
(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(use-package gitignore-mode
  :mode "\\.gitignore\\'")

(use-package rust-mode
  :pin melpa
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-r" . rust-run)
              ("C-c C-c" . 'rust-compile)
              ("C-c C-t" . 'rust-test))
  :config (progn
            (setq rust-format-on-save t)))


;;; Org Packages

(require 'init-calendar)
(require 'init-org)


;;; Email

(require 'init-mu4e)
