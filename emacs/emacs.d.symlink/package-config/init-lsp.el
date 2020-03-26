(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (rust-mode . lsp)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(provide 'init-lsp)
