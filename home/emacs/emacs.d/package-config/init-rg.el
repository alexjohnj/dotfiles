;;; init-rg.el --- Ripgrep configuration -*- lexical-binding: t -*-

(use-package rg
  :when alex/rg-available
  :commands (rg rg-project)
  :config (rg-enable-default-bindings))

(provide 'init-rg)
