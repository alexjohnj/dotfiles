(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config (progn
              (exec-path-from-shell-initialize))))

(provide 'init-exec-path-from-shell)
