(use-package ledger-mode
  :ensure t
  :mode ("\\.journal$" . ledger-mode)
  :init (progn
            (if (eq system-type 'darwin) (setq ledger-binary-path "/usr/local/bin/ledger"))
            (setq ledger-post-amount-alignment-column 60
                  ledger-post-auto-adjust-amounts t
                  ledger-highlight-xact-under-point nil)))

(use-package flycheck-ledger :ensure t :defer t)

(provide 'init-ledger-mode)
