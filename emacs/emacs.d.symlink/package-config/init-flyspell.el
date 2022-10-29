(use-package flyspell
  :diminish flyspell-mode
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :init
  (which-key-add-key-based-replacements "SPC S" "Spelling")
  :general (alex/leader-def
             "S t" #'flyspell-mode
             "S s" #'ispell-buffer
             "S S" #'flyspell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-dictionary "en_GB"
          ispell-extra-args '("--sug-mode=ultra"))))

(provide 'init-flyspell)
