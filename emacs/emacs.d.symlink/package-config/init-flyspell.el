(use-package flyspell
  :diminish flyspell-mode
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :init (progn
          (which-key-add-key-based-replacements "SPC S" "Spelling"))
  :config (progn
            (when (executable-find "aspell")
              (setq ispell-program-name "aspell"
                    ispell-dictionary "en_GB"
                    ispell-extra-args '("--sug-mode=ultra")))
            (evil-leader/set-key
              "S t" 'flyspell-mode
              "S s" 'ispell-buffer
              "S S" 'flyspell-buffer)))

(provide 'init-flyspell)
