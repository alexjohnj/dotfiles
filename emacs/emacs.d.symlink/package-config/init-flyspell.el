(use-package flyspell
  :ensure t
  :commands (flyspell-prog-mode flyspell-mode)
  :init (progn
          (setq ispell-program-name "aspell"
                ispell-dictionary "en_GB")
          (add-hook 'prog-mode-hook 'flyspell-prog-mode)
          (add-hook 'text-mode-hook 'flyspell-mode)))
(provide 'init-flyspell)
