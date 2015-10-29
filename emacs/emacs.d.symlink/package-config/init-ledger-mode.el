(use-package ledger-mode
  :ensure t
  :mode ("\\.journal$" . ledger-mode)
  :init (progn
            (if (eq system-type 'darwin) (setq ledger-binary-path "/usr/local/bin/ledger"))
            (setq ledger-post-amount-alignment-column 60
                  ledger-post-auto-adjust-amounts t
                  ledger-highlight-xact-under-point nil)
            (evil-leader/set-key-for-mode 'ledger-mode
              "mn" 'ledger-navigate-next-xact-or-directive
              "mN" 'ledger-navigate-prev-xact-or-directive
              "mp" 'ledger-display-balance-at-point
              "ma" 'ledger-add-transaction
              "md" 'ledger-delete-current-transaction
              "me" 'ledger-toggle-current-transaction
              "mc" 'ledger-toggle-current
              "my" 'ledger-copy-transaction-at-point
              "ml" 'ledger-display-ledger-stats
              "mq" 'ledger-post-align-xact
              "mr" 'ledger-reconcile
              "ms" 'ledger-sort-region
              "mt" 'ledger-insert-effective-date
              "mu" 'ledger-schedule-upcoming
              "mor" 'ledger-report
              "mos" 'ledger-report-save)))

(use-package flycheck-ledger :ensure t :defer t)

(provide 'init-ledger-mode)
