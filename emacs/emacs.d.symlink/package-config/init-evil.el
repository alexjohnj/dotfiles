(use-package evil-leader
  :init
  ;; This must bet set before the evil package is loaded for the evil-collection
  ;; package to work. evil-leader implicitly loads evil so I set it here.
  (setq evil-want-keybinding nil)
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(use-package evil
  :config
  (evil-mode 1))

(use-package undo-fu
  :bind (:map evil-normal-state-map
              ("u" . undo-fu-only-undo)
              ("C-r" . undo-fu-only-redo)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (global-set-key (kbd "s-/") #'evil-commentary)
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "fd"
                evil-escape-delay 0.1)
  (evil-escape-mode))

(provide 'init-evil)
