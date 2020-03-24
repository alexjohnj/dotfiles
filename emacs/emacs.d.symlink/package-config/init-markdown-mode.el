(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode))
  :hook (markdown-mode . flyspell-mode)
  :init (progn
          (setq markdown-italic-underscore t
                markdown-enable-math t))
  :config (progn
            (add-hook 'markdown-mode-hook 'flyspell-mode)))

(provide 'init-markdown-mode)
