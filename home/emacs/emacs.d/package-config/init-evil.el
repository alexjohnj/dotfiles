(use-package evil
  :init
  (setopt evil-want-keybinding nil)
  :config
  (setopt evil-want-fine-undo t
          evil-move-beyond-eol t)
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
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :config
  (general-def "s-/" #'evil-commentary)
  (evil-commentary-mode t))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config
  (setopt evil-escape-key-sequence "fd"
          evil-escape-delay 0.1)
  (evil-escape-mode t))

(use-package evil-cleverparens
  :hook (emacs-lisp-mode))

(provide 'init-evil)
