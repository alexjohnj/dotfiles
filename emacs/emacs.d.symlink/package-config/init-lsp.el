(use-package lsp-mode
  :commands lsp
  :init (progn
          (add-hook 'python-mode-hook #'lsp)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(provide 'init-lsp)
