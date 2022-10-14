(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :hook ((swift-mode . tree-sitter-hl-mode)
         (swift-mode . alex/swift-mode-hook)))

(use-package flycheck-swift
  :after (flycheck swift-mode)
  :config (progn
            (add-to-list 'flycheck-checkers 'swift)))

(defun alex/swift-mode-hook ()
  (setq fill-column 120))

(provide 'init-swift)
