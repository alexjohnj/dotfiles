;; -*- lexical-binding: t -*-

;; Most of this is taken from doom-emacs.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

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
(push '(background-color . "#0f0e06") default-frame-alist)

;; Make the default frame a bit bigger.
(push '(width . 140) default-frame-alist)
(push '(height . 100) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq pgtk-wait-for-event-timeout nil)

;; Compile lsp-mode in plist mode for better performance.
(setenv "LSP_USE_PLISTS" "true")
