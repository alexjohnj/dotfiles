(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package evil-escape
  :ensure t
  :config (progn
            (evil-escape-mode)))

(use-package evil-commentary
  :ensure t
  :bind (("S-/" . evil-commentary))
  :config (progn (evil-commentary-mode)))

(provide 'init-evil-mode)
