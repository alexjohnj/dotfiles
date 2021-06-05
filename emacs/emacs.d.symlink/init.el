;;; config.el -*- lexical-binding: t; -*-
;;; Basic Configuration

;; The following performance tweaks are taken from Doom Emacs.
;; Increase garbage collection threshold during init to improve performance.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(when (version< emacs-version "27")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

(defconst alex/IS-MAC (eq system-type 'darwin))
(defconst alex/IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defconst alex/IS-NATIVE-COMP (and (fboundp 'native-comp-available-p)
                                   (native-comp-available-p)))

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

;; Say hi
(setq user-full-name "Alex Jackson"
      user-mail-address "alex@alexj.org")


;; Bootstrap straight.el

(setq straight-use-package-by-default t
      straight-build-dir (format "build-%s" emacs-version)
      straight-check-for-modifications '(find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-compute-statistics t)


;;; Supporting functions/macros

(require 'cl-lib)

;; Stolen from doom-emacs
(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. Supports compound package statements (see below)
3. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                    (require package nil 'noerror))
                #'progn
              #'with-no-warnings)
            (let ((body (macroexp-progn body)))
              `(if (featurep ',package)
                   ,body
                 ;; We intentionally avoid `with-eval-after-load' to prevent
                 ;; eager macro expansion from pulling (or failing to pull) in
                 ;; autoloaded macros/packages.
                 (eval-after-load ',package ',body))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))



;;; Emacs Server/Daemon

;; Start a new Emacs server instance if one isn't running.
(require 'server)
(unless (server-running-p)
  (server-mode 1))


;;; Performance Optimisations (from doom-emacs)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a notable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; Performance on Windows is considerably worse than elsewhere, especially if
;; WSL is involved. We'll need everything we can get.
(when alex/IS-WINDOWS
  (setq w32-get-true-file-attributes nil)) ; slightly faster IO

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
;;
;; This is set up a little different to how doom-emacs sets it up.
;;
(use-package gcmh
  :diminish
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold 16777216 ; 16 MB
        gc-cons-percentage 0.1)
  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)
  (gcmh-mode))

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later, at which time it (somehow) runs very quickly.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

;; Making the initial major mode fundamental mode avoids loading in packages
;; only used for emacs-lisp-mode.
(setq initial-major-mode 'fundamental-mode)

;; lsp-mode recommends increasing this from the default value of 4K.
;; Not sure if this has any impact outside of lsp though.
(when (boundp 'read-process-output-max) ; Only available in Emacs 27+
  (setq read-process-output-max (* 1024 1024)))


;;; Core Packages

;; I've set up the configuration for core packages early in init because they're used so much later on.

;; Ensure the system PATH is picked up properly by Emacs
(use-package exec-path-from-shell
  :unless (memq system-type '(windows-nt ms-dos))
  :config
  (add-to-list 'exec-path-from-shell-variables "GPG_AGENT_INFO")
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (add-to-list 'exec-path-from-shell-variables "SSH_AGENT_PID")
  (exec-path-from-shell-initialize))

(defconst alex/rg-available (if (executable-find "rg") t nil)
  "t if the rg executable is available on this system.")

(defconst alex/fd-available  (if (executable-find "fd") t nil)
  "t if the fd executable is available on this system.")

;; Keep the modeline neat and tidy
(use-package diminish
  :commands diminish)

(require 'init-evil)

(use-package ivy
  :diminish ivy
  :config
  (evil-leader/set-key "s" 'ivy-resume)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d)"
        ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-dispatching-done)
  (define-key ivy-minibuffer-map (kbd "<C-M-return>") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map [escape] (kbd "C-g"))
  (ivy-mode))

(use-package swiper
  :after ivy
  :diminish ivy-mode)

(use-package counsel
  :after ivy
  :config
  (when alex/rg-available
    (setq counsel-grep-base-command
          "rg -i --no-heading --line-number --color never '%s' %s"))

  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (global-set-key (kbd "C-s") #'swiper)
    (global-set-key (kbd "C-s") #'counsel-grep-or-swiper))

  (global-set-key (kbd "M-x") #'counsel-M-x)
  (global-set-key (kbd "C-x C-f") #'counsel-find-file)
  (evil-leader/set-key "i" #'counsel-imenu))

(use-package prescient
  :config
  (prescient-persist-mode t))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))


;;; Editor Settings

;; Set the correct indentation level and character.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
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
(when (and alex/IS-MAC
           (fboundp 'set-fontset-font))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; Editing contextual info
(global-hl-line-mode 1)
(line-number-mode)
(column-number-mode)

;; Save backups to a temporary directory instead of the current directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Send files to the trash.
(setq delete-by-moving-to-trash alex/IS-MAC)

(use-package xcodeproj-mode
  :straight (xcodeproj-mode :type built-in))

;; Trim tidy whitespace before saving
(use-package ws-butler
  :diminish
  :config
  (ws-butler-global-mode)
  (add-hook 'xcodeproj-mode-hook '(lambda () (ws-butler-mode nil))))

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
        (set-face-attribute 'default nil :font font-name :height 140)
      (set-face-attribute 'default nil :font font-name :height 110))))

(use-package modus-themes
  :custom
  (modus-themes-intense-hl-line t)
  (modus-themes-scale-headings t)
  (modus-themes-headings '((t . rainbow-section)))
  :config
  (load-theme 'modus-operandi t))

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

(defun alex/kill-buffer-name()
  "Add the path to the current buffer's file to the kill ring"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new filename)
      (message filename))))


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
  "~" 'ansi-term)

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
                          "y" 'alex/show-buffer-name
                          "Y" 'alex/kill-buffer-name)

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
(when alex/IS-MAC
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
  :diminish
  :config
  (require 'smartparens-config)
  (sp-local-pair '(c-mode swift-mode) "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
  (smartparens-global-mode))

(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (when alex/rg-available
    (dumb-jump-prefer-searcher 'rg)))

(use-package dash-at-point
  :when alex/IS-MAC
  :bind (([f1] . dash-at-point))
  :config
  (add-to-list 'dash-at-point-mode-alist '(swift-mode . "o")))

(use-package tree-sitter
  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(require 'init-projectile)
(require 'init-flycheck)
(require 'init-rainbow-delimiters)
(require 'init-magit)
(require 'init-flyspell)
(require 'init-rg)
(require 'init-deft)
(require 'init-ediff)


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
(require 'init-dart)
(require 'init-rust)

(use-package json-mode
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

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode))

(use-package kotlin-mode
  :mode ("\\.kt\\'" . kotlin-mode))


;;; Org Packages

(require 'init-calendar)
(require 'init-org)
