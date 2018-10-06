(use-package lsp-mode
  :ensure t
  :config (progn
            (lsp-define-stdio-client lsp-python
                                     "python"
                                     #'projectile-project-root
                                     '("pyls"))
            (add-hook 'python-mode-hook (lambda ()
                                          (lsp-python-enable)))))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config (progn
            (push 'company-lsp company-backends)))

(provide 'init-lsp)
