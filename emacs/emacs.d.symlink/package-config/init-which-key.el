(use-package which-key
  :ensure t
  :defer t
  :config (progn
            (which-key-mode)
            (which-key-setup-side-window-right-bottom)
            (setq which-key-idle-delay 0.5)))
(provide 'init-which-key)
