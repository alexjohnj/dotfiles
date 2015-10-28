(use-package projectile
  :ensure t
  :config (progn
            (projectile-global-mode)
            (setq projectile-switch-project-action 'projectile-dired)))
(provide 'init-projectile)
