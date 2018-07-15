(use-package deft
  :commands (deft)
  :init (progn
          (evil-leader/set-key "n" 'deft))
  :config (progn
            (setq deft-directory "~/Documents/Notes/"
                  deft-default-extension "org"
                  deft-use-filter-string-for-filename t
                  deft-org-mode-title-prefix t)))

(provide 'init-deft)
