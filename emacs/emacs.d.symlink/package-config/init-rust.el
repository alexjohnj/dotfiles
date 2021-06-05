(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (add-hook 'rustic-mode-hook #'tree-sitter-hl-mode)
  (push 'rustic-clippy flycheck-checkers))

(provide 'init-rust)
