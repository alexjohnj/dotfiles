(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
  :diminish paredit-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
          (add-hook 'clojure-mode-hook 'enable-paredit-mode)))

(provide 'init-paredit)
