(use-package jinx
  :straight nil ;; Installed via. nix
  :diminish
  :general
  ([remap ispell-word] #'jinx-correct-word)
  (alex/leader-def
    "S s" #'jinx-correct-all
    "S n" #'jinx-next
    "S p" #'jinx-previous)
  :hook ((prog-mode . jinx-mode)
         (text-mode . jinx-mode)))

(provide 'init-spelling)
