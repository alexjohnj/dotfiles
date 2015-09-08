(use-package company
  :ensure t
  :config
  (setq company-idle-delay .1)
  (add-hook 'after-init-hook 'global-company-mode))
(provide 'init-company)
