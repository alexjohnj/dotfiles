(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-italic-underscore t)
  (setq markdown-enabke-math t)
  (add-to-list 'auto-mode-alist '("\\.markdown" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md" . gfm-mode))
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(provide 'init-markdown-mode)
