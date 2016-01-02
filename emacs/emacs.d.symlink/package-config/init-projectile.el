(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (progn
          (setq projectile-switch-project-action 'projectile-dired)
          (evil-leader/set-key
            "pb" 'projectile-switch-to-buffer
            "pd" 'projectile-find-dir
            "pf" 'projectile-find-file
            "pp" 'projectile-switch-project
            "pq" 'projectile-switch-open-project
            "pb" 'projectile-switch-to-buffer
            "pk" 'projectile-kill-buffers
            "pD" 'projectile-dired
            "pg" 'projectile-grep)
          (which-key-add-key-based-replacements "SPC p" "Project"))
  :config (progn
            (projectile-global-mode)))

(provide 'init-projectile)
