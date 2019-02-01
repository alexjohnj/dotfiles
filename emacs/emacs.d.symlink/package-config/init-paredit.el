(use-package paredit
  :ensure t
  :diminish  paredit-mode
  :hook (emacs-lisp-mode . enable-paredit-mode))

(provide 'init-paredit)
