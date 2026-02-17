;;; init-diagnostics.el --- Flymake configuration -*- lexical-binding: t -*-

(use-package flymake
  :ensure nil ; Ensure the built in flymake package is used.
  :hook ((prog-mode . flymake-mode))
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
