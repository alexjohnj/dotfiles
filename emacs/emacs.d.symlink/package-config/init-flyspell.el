(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :commands (flyspell-prog-mode flyspell-mode)
  :init (progn
          (setq ispell-program-name "aspell"
                ispell-dictionary "en_GB")
          (add-hook 'prog-mode-hook 'flyspell-prog-mode)
          (add-hook 'text-mode-hook 'flyspell-mode)
          (evil-leader/set-key
            "S t" 'flyspell-mode
            "S s" 'ispell-buffer
            "S S" 'flyspell-buffer)
          (which-key-add-key-based-replacements "SPC S" "Spelling")))
(provide 'init-flyspell)
