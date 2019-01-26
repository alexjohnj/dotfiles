(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :diminish company-mode
  :config (setq company-idle-delay 0
                company-minimum-prefix-length 0
                company-tooltip-idle-delay 0
                company-show-numbers t ;; Show numbers next to each completion option
                company-require-match nil ;; Allow non-matching input
                company-dabbrev-downcase nil ;; Don't downcase completions
                company-dabbrev-ignore-case t ;; Ignore case when completing
                company-dabbrev-code-ignore-case t
                company-dabbrev-code-everywhere t))

(provide 'init-company)
