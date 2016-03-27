(use-package spaceline
  :ensure t
  :config (progn
            (require 'spaceline-config)
            (setq powerline-default-separator 'bar)
            (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
            (spaceline-spacemacs-theme)))

(provide 'init-spaceline)
