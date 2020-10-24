(use-package deft
  :commands (deft)
  :init (evil-leader/set-key "n" #'deft)
  :config
  (add-to-list 'evil-emacs-state-modes 'deft-mode)
  (setq deft-directory "~/Documents/Notes/"
        deft-default-extension "org"
        deft-use-filter-string-for-filename t
        deft-org-mode-title-prefix t))

(provide 'init-deft)
