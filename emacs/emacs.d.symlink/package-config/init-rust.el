(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (push 'rustic-clippy flycheck-checkers))

(provide 'init-rust)
