(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config (progn
              (exec-path-from-shell-initialize)
              (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))))

(provide 'init-exec-path-from-shell)
