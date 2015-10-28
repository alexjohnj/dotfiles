(use-package ledger-mode
  :ensure t
  :defer t
  :mode ("\\.journal$" . ledger-mode)
  :config (progn
            (if (eq system-type 'darwin) (setq ledger-binary-path "/usr/local/bin/ledger"))
            (setq ledger-post-amount-alignment-column 60)
            (setq ledger-post-auto-adjust-amounts t)
            (setq ledger-highlight-xact-under-point nil)))

(use-package flycheck-ledger :ensure t :defer t)

(provide 'init-ledger-mode)
