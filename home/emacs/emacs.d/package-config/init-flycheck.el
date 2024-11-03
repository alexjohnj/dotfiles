(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :general (alex/leader-def
             "e c" #'flycheck-buffer
             "e n" #'flycheck-next-error
             "e p" #'flycheck-previous-error
             "e l" #'flycheck-list-errors
             "e t" #'flycheck-mode
             "e v" #'flycheck-verify-setup)
  :init
  (which-key-add-key-based-replacements "SPC e" "Flycheck"))

(provide 'init-flycheck)
