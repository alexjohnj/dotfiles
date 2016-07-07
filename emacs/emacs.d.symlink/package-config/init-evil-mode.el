;;; init-evil-mode.el --- Configuration for evil-mode and related packages
;;; Code:

(use-package evil-leader
  :ensure t
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (global-evil-leader-mode)))

(use-package evil
  :ensure t
  :after evil-leader
  :diminish undo-tree-mode
  :config (progn (evil-mode 1)))

(use-package evil-escape
  :ensure t
  :after evil
  :diminish evil-escape-mode
  :config (progn
            (evil-escape-mode)))

(use-package evil-commentary
  :ensure t
  :after evil
  :diminish evil-commentary-mode
  :bind (("S-/" . evil-commentary))
  :init (progn
          (evil-commentary-mode)))

(provide 'init-evil-mode)
;;; init-evil-mode.el ends here
