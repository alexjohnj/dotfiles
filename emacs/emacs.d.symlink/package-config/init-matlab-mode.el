(use-package matlab-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode)))

(provide 'init-matlab-mode)
