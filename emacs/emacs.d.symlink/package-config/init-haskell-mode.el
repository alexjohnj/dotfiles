(use-package haskell-mode
  :ensure t
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (custom-set-variables '(haskell-stylish-on-save t))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(provide 'init-haskell-mode)
