(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (swift-mode . lsp)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))

(provide 'init-lsp)
