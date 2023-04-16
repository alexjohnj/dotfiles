;; -*- lexical-binding: t; -*-

(use-package corfu
  :straight (:files (:defaults "extensions/*")) ;; Loads additional extensions from repo
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1)

  ;; Wait 0.5s to show the popup and then 1s to update it.
  (setq corfu-popupinfo-delay '(0.5 . 1)))

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

(use-package eglot
  :commands eglot)

(provide 'init-code-completion)
