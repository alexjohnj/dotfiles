(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :hook ((swift-mode . tree-sitter-hl-mode)))

(use-package flycheck-swift
  :after (flycheck swift-mode)
  :config (progn
            (add-to-list 'flycheck-checkers 'swift)))

(use-package lsp-sourcekit
  :after (lsp-mode swift-mode))

(provide 'init-swift)
