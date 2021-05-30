(use-package magit
  :commands (magit-dispatch
             magit-status
             magit-file-dispatch)
  :init
  (alex/evil-leader--prefix "g"
                            "g" 'magit-dispatch
                            "s" 'magit-status
                            "f" 'magit-file-dispatch)
  (which-key-add-key-based-replacements "SPC g" "Magit")
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-magit)
