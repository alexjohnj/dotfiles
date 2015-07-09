(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-dired))
(provide 'init-projectile)
