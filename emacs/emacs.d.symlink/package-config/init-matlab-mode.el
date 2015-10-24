(use-package matlab-mode
  :ensure t
  :defer t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
            (setq matlab-indent-level 2)))

(provide 'init-matlab-mode)
