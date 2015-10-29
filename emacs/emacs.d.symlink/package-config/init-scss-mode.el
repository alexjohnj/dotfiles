(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'" . scss-mode)
  :init (progn
          (setq scss-compile-at-save nil)))

(provide 'init-scss-mode)
