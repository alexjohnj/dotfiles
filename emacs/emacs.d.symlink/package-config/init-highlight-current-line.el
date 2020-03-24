(use-package highlight-current-line
  :config (progn
            (global-hl-line-mode t)
            (setq highlight-current-line-globally t)
            (setq highlight-current-line-high-faces nil)
            (setq highlight-current-line-whole-line nil)
            (setq hl-line-face (quote highlight))))

(provide 'init-highlight-current-line)
