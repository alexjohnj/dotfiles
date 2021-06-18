(use-package magit
  :commands (magit-dispatch
             magit-status
             magit-file-dispatch)
  :general
  (alex/leader-def
    "g g" #'magit-dispatch
    "g s" #'magit-status
    "g f" #'magit-file-dispatch)
  :init
  (which-key-add-key-based-replacements "SPC g" "Magit")
  :config
  (setq magit-section-initial-visibility-alist
        '(([unpushed status] . show)
          ([unstaged status] . show)
          ([untracked status] . show)))
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-magit)
