(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode))

(use-package flycheck-swift
  :ensure t
  :after (flycheck swift-mode)
  :config (progn
            (add-to-list 'flycheck-checkers 'swift)))


(provide 'init-swift)
