(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package evil-escape
  :ensure t
  :config (progn
            (evil-escape-mode)))

(provide 'init-evil-mode)
