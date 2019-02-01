(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :init (progn
          (setq ispell-program-name "aspell"
                ispell-dictionary "en_GB")
          (evil-leader/set-key
            "S t" 'flyspell-mode
            "S s" 'ispell-buffer
            "S S" 'flyspell-buffer)
          (which-key-add-key-based-replacements "SPC S" "Spelling")))
(provide 'init-flyspell)
