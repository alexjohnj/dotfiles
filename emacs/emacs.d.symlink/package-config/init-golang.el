(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :ensure t
  :config (add-hook 'before-save-hook #'gofmt-before-save))
(provide 'init-golang)
