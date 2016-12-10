(use-package company
  :ensure t
  :diminish company-mode
  :commands (global-company-mode)
  :init (progn (add-hook 'after-init-hook 'global-company-mode))
  :config (progn
            (setq company-idle-delay 0.3
                       company-minimum-prefix-length 2
                       company-show-numbers t
                       company-dabbrev-downcase nil
                       company-dabbrev-ignore-case t
                       company-dabbrev-code-ignore-case t
                       company-dabbrev-code-everywhere t)))

(provide 'init-company)
