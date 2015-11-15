(use-package evil-leader
  :ensure t
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (global-evil-leader-mode)))

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :config (progn
            (evil-mode 1)))

(use-package evil-escape
  :ensure t
  :diminish evil-escape-mode
  :config (progn
            (evil-escape-mode)))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :bind (("S-/" . evil-commentary))
  :init (progn
          (evil-commentary-mode)))

(provide 'init-evil-mode)
