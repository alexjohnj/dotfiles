(use-package matlab
  :ensure matlab-mode
  :mode ("\\.m\\'" . matlab-mode)
  :config (progn
            (setq matlab-indent-level 2
                  matlab-indent-function-body t
                  matlab-return-add-semicolon t)
            (evil-leader/set-key-for-mode 'matlab-mode
              "m c c" 'matlab-ispell-comments
              "m c s" 'matlab-ispell-strings
              "m s s" 'matlab-shell
              "m s <" 'matlab-show-matlab-shell-buffer
              "m s g" 'matlab-shell-save-and-go
              "m s v" 'matlab-shell-describe-variable
              "m s h" 'matlab-shell-describe-command
              "m s e" 'matlab-shell-last-error)))

(provide 'init-matlab-mode)
