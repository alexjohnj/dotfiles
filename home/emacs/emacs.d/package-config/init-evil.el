;;; init-evil.el --- Evil mode configuration -*- lexical-binding: t -*-

(use-package evil
  :demand t
  :ensure (:wait t)
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

(use-package evil-collection
  :demand t
  :ensure (:wait t)
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package undo-fu
  :general
  ('normal "u" #'undo-fu-only-undo
           "C-r" #'undo-fu-only-redo))

(use-package evil-commentary
  :hook (alex-first-input . evil-commentary-mode)
  :config
  (general-def "s-/" #'evil-commentary))

(use-package evil-surround
  :hook (alex-first-input . global-evil-surround-mode))

(use-package evil-escape
  :hook (alex-first-input . evil-escape-mode)
  :diminish evil-escape-mode
  :custom
  (evil-escape-key-sequence "fd")
  (evil-escape-delay 0.1))

(use-package evil-cleverparens
  :hook (emacs-lisp-mode))

(provide 'init-evil)
