(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (dart-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode-on))
  :config
  (setq lsp-enable-on-type-formatting nil)
  (general-def lsp-mode-map
    "M-RET" #'lsp-execute-code-action))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-peek-enable nil))

(provide 'init-lsp)
