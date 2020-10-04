(use-package magit
  :commands (magit-dispatch-popup
             magit-status
             magit-blame)
  :init
  (alex/evil-leader--prefix "g"
                            "g" 'magit-dispatch-popup
                            "s" 'magit-status
                            "b" 'magit-blame)
  (which-key-add-key-based-replacements "SPC g" "Magit")
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after (magit))

(provide 'init-magit)
