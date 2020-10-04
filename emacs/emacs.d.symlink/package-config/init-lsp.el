(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (dart-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode-on))
  :config
  (setq lsp-enable-on-type-formatting nil))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))

(provide 'init-lsp)
