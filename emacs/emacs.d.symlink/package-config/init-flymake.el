(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :general (alex/leader-def
             "e c" #'flymake-start
             "e n" #'flymake-goto-next-error
             "e p" #'flymake-goto-prev-error
             "e l" #'flymake-show-buffer-diagnostics
             "e t" #'flymake-mode
             "e v" #'flymake-running-backends)
  :init
  (which-key-add-key-based-replacements "SPC e" "Flymake"))

(provide 'init-flymake)
