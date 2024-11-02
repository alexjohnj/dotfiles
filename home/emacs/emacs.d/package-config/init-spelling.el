(use-package jinx
  :straight nil ;; Installed via. nix
  :diminish
  :general
  ([remap ispell-word] #'jinx-correct)
  (alex/leader-def "S s" #'jinx-correct-all)
  :hook ((prog-mode . jinx-mode)
         (text-mode . jinx-mode)))

(provide 'init-spelling)
