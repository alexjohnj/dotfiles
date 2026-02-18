;; -*- lexical-binding: t -*-

(defconst alex/IS-MAC (eq system-type 'darwin))
(defconst alex/IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defconst alex/IS-NATIVE-COMP (and (fboundp 'native-comp-available-p)
                                   (native-comp-available-p)))

;;; Custom Hooks (from smallwat3r/emacs)
(defvar alex-first-input-hook nil
  "Transient hook run before the first user input.")

(defvar alex-first-file-hook nil
  "Transient hook run before the first file is opened.")

(defvar alex-first-buffer-hook nil
  "Transient hook run before the first buffer switch.")

(defun alex--run-first-input ()
  "Run `alex-first-input-hook' once then remove trigger."
  (when alex-first-input-hook
    (run-hooks 'alex-first-input-hook)
    (setq alex-first-input-hook nil)
    (remove-hook 'pre-command-hook #'alex--run-first-input)))

(defun alex--run-first-file ()
  "Run `alex-first-file-hook' once then remove trigger."
  (when alex-first-file-hook
    (run-hooks 'alex-first-file-hook)
    (setq alex-first-file-hook nil)
    (remove-hook 'find-file-hook #'alex--run-first-file)))

(defun alex--run-first-buffer (&rest _)
  "Run `alex-first-buffer-hook' once then remove trigger."
  (when alex-first-buffer-hook
    (run-hooks 'alex-first-buffer-hook)
    (setq alex-first-buffer-hook nil)
    (remove-hook 'window-buffer-change-functions #'alex--run-first-buffer)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'pre-command-hook #'alex--run-first-input)
            (add-hook 'find-file-hook #'alex--run-first-file)
            (add-hook 'window-buffer-change-functions #'alex--run-first-buffer)))

;;; Performance Tweaks (mostly from Doom Emacs)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; Suppress compilation messages
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. I'm using elpaca for package
;; management so this isn't needed.
(setq package-enable-at-startup nil
      package-quickstart nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(background-color . "#000000") default-frame-alist)

;; Disable expensive GUI elements early
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; Make the default frame a bit bigger.
(push '(width . 140) default-frame-alist)
(push '(height . 100) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq pgtk-wait-for-event-timeout nil)


;; Temporarily disable the file handler list during startup for improved performance.
(defvar alex--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist alex--file-name-handler-alist)))

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

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later, at which time it (somehow) runs very quickly.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

;; lsp-mode recommends increasing this from the default value of 4K.
;; Not sure if this has any impact outside of lsp though.
(when (boundp 'read-process-output-max) ; Only available in Emacs 27+
  (setq read-process-output-max (* 1024 1024)))

;; Compile lsp-mode in plist mode for better performance.
(setenv "LSP_USE_PLISTS" "true")
