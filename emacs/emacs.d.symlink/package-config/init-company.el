(use-package company
  :ensure t
  :config (progn (setq company-idle-delay 0.1
                        company-minimum-prefix-length 2
                        company-show-numbers t)
                 (add-hook 'after-init-hook 'global-company-mode)))

(provide 'init-company)
