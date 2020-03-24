(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode))

(use-package flycheck-swift
  :after (flycheck swift-mode)
  :config (progn
            (add-to-list 'flycheck-checkers 'swift)))


(provide 'init-swift)
