(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (progn
          (which-key-setup-side-window-right-bottom)
          (setq which-key-idle-delay 0.3)
          (which-key-mode)))

(provide 'init-which-key)