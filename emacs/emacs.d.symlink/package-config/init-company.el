(use-package company
  :hook (after-init . global-company-mode)
  :diminish company-mode
  :config (setq company-idle-delay 0
                company-minimum-prefix-length 1
                company-tooltip-idle-delay 0
                company-show-numbers t ;; Show numbers next to each completion option
                company-require-match 'never ;; Allow non-matching input
                company-dabbrev-other-buffers nil ;; Don't use other buffers as a completion source
                company-dabbrev-downcase nil ;; Don't downcase completions
                company-dabbrev-ignore-case nil))

(provide 'init-company)
