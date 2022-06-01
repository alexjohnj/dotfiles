(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (add-hook 'rustic-mode-hook #'tree-sitter-hl-mode)
  (setq rustic-format-on-save t
        rustic-lsp-setup-p nil)
  (push 'rustic-clippy flycheck-checkers)

  (alex/leader-local-def rustic-mode-map
    "b" #'rustic-cargo-build
    "c" #'rustic-cargo-check
    "t a" #'rustic-cargo-test
    "t t" #'rustic-cargo-current-test
    "t f" #'rustic-cargo-test-rerun))

(provide 'init-rust)
