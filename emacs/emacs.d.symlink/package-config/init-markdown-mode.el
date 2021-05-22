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

(provide 'init-markdown-mode)
