(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config (progn
              (exec-path-from-shell-initialize)
              (exec-path-from-shell-copy-env "GPG_AGENT_INFO")
              (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
              (exec-path-from-shell-copy-env "SSH_AGENT_PID"))))

(provide 'init-exec-path-from-shell)
