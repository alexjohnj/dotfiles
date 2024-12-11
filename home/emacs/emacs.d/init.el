;;; config.el -*- lexical-binding: t; -*-
;;; Basic Configuration

;; The following performance tweaks are taken from Doom Emacs.

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
(add-hook 'emacs-startup-hook #'(lambda ()
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
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(when (version< emacs-version "29.1")
  (straight-use-package 'use-package))

(setopt use-package-compute-statistics t
        use-package-enable-imenu-support t)

(use-package esup
  :commands (esup)
  :config
  ;; Fixes incompatibility with symlinks and straight.el
  (setq esup-depth 0))


;;; Emacs Server/Daemon

(defun alex/start-server-if-needed ()
  (unless (server-running-p)
    (server-start)))

(use-package server
  ;; Start a new Emacs server instance if one isn't running.
  :commands (server-running-p)
  :hook ((after-init . alex/start-server-if-needed)))


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
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect)
  (setopt gcmh-idle-delay 'auto
          gcmh-auto-idle-delay-factor 10
          gcmh-high-cons-threshold (* 16 1024 1024)) ; 16 MB
  (gcmh-mode t))

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
  :when (memq system-type '(darwin))
  :config
  (add-to-list 'exec-path-from-shell-variables "GPG_AGENT_INFO")
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (add-to-list 'exec-path-from-shell-variables "SSH_AGENT_PID")
  (exec-path-from-shell-initialize))

(defconst alex/rg-available (if (executable-find "rg") t nil)
  "t if the rg executable is available on this system.")

(defconst alex/fd-available  (if (executable-find "fd") t nil)
  "t if the fd executable is available on this system.")

(defconst alex/trash-available (if (executable-find "trash") t nil)
  "t if the trash executable is available on this system.")

(defconst alex/emacs-lsp-booster-available
  (if (executable-find "emacs-lsp-booster") t nil))

;; Keep the modeline neat and tidy
(use-package diminish
  :commands diminish)

(use-package general
  :config
  (general-create-definer alex/leader-def
    :states '(normal motion insert emacs)
    :prefix "SPC"
    :keymaps 'override
    :non-normal-prefix "M-SPC")

  (general-create-definer alex/leader-local-def
    :states '(normal motion insert emacs)
    :prefix "SPC m"
    :keymaps 'override
    :non-normal-prefix "M-SPC m"))

(require 'init-evil)

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(use-package highlight-indent-guides
  :diminish
  :hook ((text-mode . highlight-indent-guides-mode)
         (prog-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))


;;; Completion Frameworks

(use-package vertico
  :config
  (general-def vertico-map
    [escape] "C-g")
  (vertico-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package corfu
  :straight (:files (:defaults "extensions/*")) ;; Loads additional extensions from repo
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :general
  (:states '(insert) "C-SPC" #'completion-at-point)
  :config
  (setopt corfu-auto t
          corfu-cycle t
          corfu-on-exact-match nil
          corfu-auto-delay 0.1
          corfu-auto-prefix 2)

  ;; Wait 0.5s to show the popup and then 1s to update it.
  (setopt corfu-popupinfo-delay '(0.5 . 0.2)))

;; Adds icons to the margins of the Corfu completion popup.
(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-use-icons nil) ;; Use text based icons instead of SVGs.
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Provides additional completion at point functions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (setq cape-dabbrev-check-other-buffers nil))

;; The marginalia package adds annotations next to completion results.
(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :general
  ([remap switch-to-buffer] #'consult-buffer
   [remap imenu] #'consult-imenu
   [remap apropos] #'consult-apropos
   [remap man] #'consult-man))

(use-package ctrlf
  :defer nil
  :general
  (:keymaps 'ctrlf-minibuffer-mode-map [escape] #'ctrlf-cancel)
  :config
  (ctrlf-mode))

(use-package eglot
  :disabled t
  :straight nil ; Built in as of Emacs 29.1
  :commands (eglot eglot-ensure)
  :general
  ([(super .)] #'eglot-code-actions)
  :config
  (setq eglot-confirm-server-initiated-edits nil
        eglot-autoshutdown t)
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  (fset #'jsonrpc--log-event #'ignore); massive perf boost---don't log every event
  )

(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :when alex/emacs-lsp-booster-available
  :after eglot
  :config
  (eglot-booster-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-use-plists t)
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               lsp-use-plists
               (not (functionp 'json-rpc-connection))
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :general
  ("s-." #'lsp-execute-code-action
   [f2] #'lsp-rename)
  :config
  (when alex/emacs-lsp-booster-available
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

  ;; General
  (setopt lsp-enable-suggest-server-download nil
          lsp-keep-workspace-alive nil)

  ;; Editing
  (setopt lsp-enable-folding nil
          lsp-enable-text-document-color nil
          lsp-semantic-tokens-enable nil)

  ;; Completion
  (setopt lsp-completion-provider :none ;; Completion is provided by Corfu/CAPF
          lsp-completion-enable t)

  ;; Diagnostics
  (setopt lsp-diagnostics-provider :flycheck)

  ;; UI
  (setopt lsp-headerline-breadcrumb-enable nil
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil))

(use-package yasnippet
  :hook ((lsp-mode . yas-minor-mode)))


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
          #'(lambda () (setq show-trailing-whitespace t)))

;; Enable emoji, and stop the UI from freezing when trying to display them on a Mac.
(when (and alex/IS-MAC
           (fboundp 'set-fontset-font))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; Editing contextual info
(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; Save backups to a temporary directory instead of the current directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Send files to the correct trash folder on macOS.
(when alex/IS-MAC
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"))

(when alex/trash-available
  (defun system-move-file-to-trash (file)
    (call-process (executable-find "trash") nil 0 nil file)))

(use-package xcodeproj-mode
  :straight (xcodeproj-mode :type built-in))

(when alex/IS-MAC
  (defun alex/xed ()
    "Open Xcode in the current working directory."
    (interactive)
    (start-process "xed" nil "xed" ".")))

;; Apheleia provides a way to format on save asynchronously without messing up
;; the position of the cursor.
(use-package apheleia
  :defer nil ;; This package is well optimised so doesn't need deferring
  :diminish
  :general
  ("s-I" #'apheleia-format-buffer)
  :config
  (setq apheleia-remote-algorithm 'local)
  ;; Global mode is optimised to not load until a file is saved.
  (apheleia-global-mode t))

;; Trim tidy whitespace before saving
(use-package ws-butler
  :diminish
  :config
  (ws-butler-global-mode)
  (add-hook 'xcodeproj-mode-hook #'(lambda () (ws-butler-mode nil))))

;; Highlight matching parentheses
(use-package paren
  :init (progn
          (setq show-paren-delay 0
                show-paren-style 'parenthesis))
  :hook (prog-mode . show-paren-mode))

;; Pretty print line-feed characters. I think the only place I use line-feeds is
;; in this file!
(use-package page-break-lines
  :diminish
  :hook (emacs-lisp-mode . page-break-lines-mode))

;; Unfill text
(use-package unfill
  :general
  ("C-M-q" #'unfill-paragraph))

;; Aggressively indent code in certain modes
(use-package aggressive-indent
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; Configure tramp
(use-package tramp
  :defer t
  :straight nil
  :config
  (setq tramp-verbose 5)
  (add-to-list 'tramp-default-user-alist
               '("ssh" "munchlax\\.local\\'" "root")))

(use-package direnv
  :config
  (direnv-mode))

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))


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
(defun alex/set-font ()
  (let ((font-name "Zed Mono"))
    (if (member font-name (font-family-list))
        (if (memq window-system '(mac ns)) ; Font scaling is a bit different between
                                        ; macOS and other platforms.
            (set-face-attribute 'default nil :font font-name :height 140 :weight 'regular)
          (set-face-attribute 'default nil :font font-name :height 110))
      (warn "Font %s is not installed. Using the default font." font-name))))

;; Hook frame creation so the font is set when Emacs is running in server mode.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame (alex/set-font))))

;; Set the font immediately for when Emacs isn't running in server mode.
(alex/set-font)

(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-autumn ef-frost))
  (load-theme (car ef-themes-to-toggle) t))

;; Match the fringe colour to the theme's background colour
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; Hide the titlebar when running on macOS
(when alex/IS-MAC
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq ns-use-proxy-icon nil
        frame-title-format nil))


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

(defun alex/copy-buffer-name (arg)
  "Add the path to the current buffer's file to the kill ring."
  (interactive "P")
  (let ((filename (if arg
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message filename))))


;;; Basic Keybindings

(use-package emacs
  :preface
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))
  :config
  (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim))

(alex/leader-def "i" #'imenu)

;; Buffer Management.
(alex/leader-def :infix "b"
  "b" #'switch-to-buffer
  "d" #'kill-current-buffer
  "k" #'kill-buffer
  "l" #'list-buffers)

;; File Management
(alex/leader-def :infix "f"
  "f" #'find-file
  "d" #'dired
  "D" #'alex/delete-file-and-buffer
  "R" #'alex/rename-current-buffer-file
  "s" #'evil-write
  "S" #'evil-write-all
  "y" #'alex/show-buffer-name
  "Y" #'alex/copy-buffer-name)

;; Window Management
(alex/leader-def :infix "w"
  "=" #'balance-windows
  "c" #'delete-window
  "C" #'delete-other-windows
  "h" #'evil-window-left
  "H" #'evil-window-move-far-left
  "j" #'evil-window-down
  "J" #'evil-window-move-very-bottom
  "k" #'evil-window-up
  "K" #'evil-window-move-very-top
  "l" #'evil-window-right
  "L" #'evil-window-move-far-right
  "s" #'split-window-below
  "v" #'split-window-right)

(use-package ace-window
  :commands ace-window
  :general
  (alex/leader-def :infix "w"
    "w" #'ace-window
    "d" #'ace-delete-window
    "S" #'ace-swap-window))

;; Help System
(alex/leader-def :infix "h"
  "f" #'describe-function
  "m" #'describe-mode
  "v" #'describe-variable
  "b" #'describe-bindings
  "p" #'describe-package
  "i" #'info
  "M" #'man
  "a a" #'apropos
  "a c" #'apropos-command
  "a d" #'apropos-documentation
  "a v" #'apropos-variable
  "a o" #'apropos-user-option)

;; Text Editing
(alex/leader-def :infix "x"
  "u"   #'downcase-region
  "U"   #'upcase-region
  "a r" #'align-regexp
  "d w" #'delete-trailing-whitespace
  "i r" #'indent-region
  "i b" #'alex/indent-buffer
  "C" #'alex/toggle-reading-mode)

;; Elisp Editing
(alex/leader-local-def
  "e b" #'eval-buffer
  "e r" #'eval-region
  "e e" #'eval-last-sexp)

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
  (general-unbind [(super q)])
  (general-def
    [(super a)] #'mark-whole-buffer
    [(super v)] #'yank
    [(super s)] #'save-buffer
    [(super w)] #'delete-frame
    [(super n)] #'make-frame
    [(super z)] #'undo
    [(super ctrl f)] #'toggle-frame-fullscreen))


;;; Core Packages

(use-package smartparens
  :diminish
  :hook
  ((prog-mode text-mode markdown-mode) . smartparens-mode)
  ((emacs-lisp-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  ;; Xcode style insertion of a newline between pairs
  ;; Source: https://xenodium.com/emacs-smartparens-auto-indent/
  (defun alex/indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((alex/indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((alex/indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((alex/indent-between-pair "RET"))))

(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (when alex/rg-available
    (dumb-jump-prefer-searcher 'rg)))

(use-package eldoc
  :straight nil
  :diminish
  :bind (([f1] . eldoc-doc-buffer))
  :custom
  eldoc-idle-delay 0.25)

(use-package eldoc-box
  :diminish eldoc-box-hover-mode
  :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package rainbow-mode
  :diminish
  :hook ((prog-mode . rainbow-mode)))

(require 'init-projectile)
(require 'init-flycheck)
(require 'init-rainbow-delimiters)
(require 'init-magit)
(require 'init-spelling)
(require 'init-rg)
(require 'init-deft)
(require 'init-ediff)


;;; Language Packages

(require 'init-languages)


;;; Org Packages

(require 'init-org)

(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time))) gcs-done)
