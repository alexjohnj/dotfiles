(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (add-hook 'swift-mode-hook #'tree-sitter-hl-mode))

(use-package flycheck-swift
  :after (flycheck swift-mode)
  :config (progn
            (add-to-list 'flycheck-checkers 'swift)))


(provide 'init-swift)
