(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init (progn
          (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
          (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

(provide 'init-rainbow-delimiters)
