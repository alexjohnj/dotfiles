(use-package ledger-mode
  :ensure t
  :config
  (if (eq system-type 'darwin) (setq ledger-binary-path "/usr/local/bin/ledger"))
  (add-to-list 'auto-mode-alist '("\\.journal$" . ledger-mode))
  (setq ledger-post-auto-adjust-amounts t)
  (setq ledger-highlight-xact-under-point nil))

(provide 'init-ledger-mode)
