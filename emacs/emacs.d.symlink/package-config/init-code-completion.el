;; -*- lexical-binding: t; -*-

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :config
  ;; TODO: How do I enable corfu-indexed-mode?
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1))

;; Adds icons to the margins of the Corfu completion popup.
(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-use-icons nil) ;; Use text based icons instead of SVGs.
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Adds a documentation popup next to corfu completion candidates
(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :config
  (setq corfu-doc-delay 0.2))

;; Provides additional completion at point functions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (setq cape-dabbrev-check-other-buffers nil))

(use-package eglot
  :commands eglot)

(provide 'init-code-completion)
