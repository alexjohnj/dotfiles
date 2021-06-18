(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-want-fine-undo t)
  (alex/leader-def "SPC" #'evil-buffer)
  (general-def
    "s-[" #'evil-prev-buffer
    "s-]" #'evil-next-buffer)
  (evil-mode 1))

(use-package undo-fu
  :general
  ('normal "u" #'undo-fu-only-undo
           "C-r" #'undo-fu-only-redo))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :config
  (general-def "s-/" #'evil-commentary)
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
