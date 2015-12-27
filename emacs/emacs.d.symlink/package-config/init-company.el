(use-package company
  :ensure t
  :diminish company-mode
  :config (progn (setq company-idle-delay 0.1
                        company-minimum-prefix-length 2
                        company-show-numbers t)
                 (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-emoji
  :ensure t
  :init (progn
          (add-to-list 'company-backends 'company-emoji)))

(provide 'init-company)
