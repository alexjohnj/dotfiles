(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode))

(use-package flycheck-swift
  :ensure t
  :after (flycheck)
  :config (flycheck-swift-setup))

(provide 'init-swift)
