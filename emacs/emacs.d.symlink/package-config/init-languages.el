;; init-languages -- Programming Language Configuration

(require 'init-beancount-mode)

(use-package dockerfile-mode)

(use-package fish-mode)

(use-package git-modes
  :mode (("\\.gitignore\\'" . gitignore-mode)))

(use-package gcode-mode)

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.apns\\'" . json-mode)))

(use-package kotlin-mode)

(require 'init-latex)

(use-package lua-mode)

(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode))
  :hook (markdown-mode . flyspell-mode)
  :init
  (setq markdown-italic-underscore t
        markdown-fontify-code-blocks-natively t
        markdown-enable-math t)
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package ruby-mode
  :mode "/\\(Gem\\|Fast\\|App\\|Match\\|Pod\\)file")

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-format-on-save nil
        rustic-lsp-setup-p nil
        rustic-lsp-client 'eglot)
  (push 'rustic-clippy flycheck-checkers)

  (alex/leader-local-def rustic-mode-map
    "b" #'rustic-cargo-build
    "c" #'rustic-cargo-check
    "t a" #'rustic-cargo-test
    "t t" #'rustic-cargo-current-test
    "t f" #'rustic-cargo-test-rerun))

(use-package swift-mode
  :hook
  (swift-mode . (lambda ()
                  (setq fill-column 120))))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-engine-detection t
        web-mode-enable-current-element-highlight t)

  (defun alex/web-mode--configure-eex-pairing ()
    (sp-with-modes '(web-mode)
      (sp-local-pair "<% " " %>" :insert "C-b %")
      (sp-local-pair "<%= " " %>" :insert "C-b =")
      (sp-local-pair "<%# " " %>" :insert "C-b #")))

  (add-hook 'web-mode-hook #'alex/web-mode--configure-eex-pairing)
  (add-hook 'web-mode-hook #'aggressive-indent-mode))

(use-package yaml-mode
  :hook ((yaml-mode . highlight-indent-guides-mode)))

(provide 'init-languages)
