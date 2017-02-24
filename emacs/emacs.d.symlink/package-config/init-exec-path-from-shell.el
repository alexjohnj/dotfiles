(use-package exec-path-from-shell
  :ensure t
  :config (progn
            (add-to-list 'exec-path-from-shell-variables "GPG_AGENT_INFO")
            (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
            (add-to-list 'exec-path-from-shell-variables "SSH_AGENT_PID")
            (exec-path-from-shell-initialize)))

(provide 'init-exec-path-from-shell)
