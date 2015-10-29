(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown" . gfm-mode)
         ("\\.md" . gfm-mode))
  :init (progn
          (setq markdown-italic-underscore t
                markdown-enable-math t))
  :config (progn
            (add-hook 'markdown-mode-hook 'flyspell-mode)))

(provide 'init-markdown-mode)
