(use-package powerline
  :ensure t
  :config (progn
            (when (display-graphic-p)
              (setq powerline-default-separator 'contour))
            (powerline-default-theme)))

(provide 'init-powerline)
