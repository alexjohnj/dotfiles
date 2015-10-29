(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :init (progn
          (setq matlab-indent-level 2)
          (evil-leader/set-key-for-mode 'matlab-mode
            "mcc" 'matlab-ispell-comments
            "mcs" 'matlab-ispell-strings
            "ms<" 'matlab-show-matlab-shell-buffer
            "msg" 'matlab-shell-save-and-go
            "msv" 'matlab-shell-describe-variable
            "msh" 'matlab-shell-describe-command
            "mse" 'matlab-shell-last-error)))

(provide 'init-matlab-mode)
