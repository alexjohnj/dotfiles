(use-package flycheck
  :disabled t
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

(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :ensure nil ; Ensure the built in flymake package is used.
  :general (alex/leader-def
             "e c" #'flymake-start
             "e n" #'flymake-goto-next-error
             "e p" #'flymake-goto-prev-error
             "e L" #'flymake-show-buffer-diagnostics
             "e l" #'consult-flymake
             "e t" #'flymake-mode
             "e v" #'flymake-running-backends)
  :init
  (which-key-add-key-based-replacements "SPC e" "Flymake"))

(provide 'init-diagnostics)
