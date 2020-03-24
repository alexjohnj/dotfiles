(use-package ag
  :commands (ag)
  :config (progn
            (evil-define-key 'motion ag-mode-map
              "j" 'compilation-next-error
              "k" 'compilation-previous-error
              "g" 'recompile
              "q" 'quit-window
              "Q" '(lambda () (interactive) (quit-window t (get-buffer-window)))
              "RET" 'compile-goto-error
              "s" 'compilation-display-error)))

(provide 'init-ag)
