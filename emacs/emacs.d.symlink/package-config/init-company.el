(use-package company
  :ensure t
  :diminish company-mode
  :commands (global-company-mode)
  :init (progn (add-hook 'after-init-hook 'global-company-mode))
  :config (progn
            (setq company-idle-delay 0.1
                       company-minimum-prefix-length 2
                       company-show-numbers t)))

(provide 'init-company)
