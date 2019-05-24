(use-package magit
  :ensure t
  :commands (magit-status
             magit-log-other
             magit-commit-create
             magit-stage-file)
  :init (progn
          (evil-leader/set-key
            "g s" 'magit-status
            "g S" 'magit-stage-file
            "g C" 'magit-commit-create
            "g l" 'magit-log-all-branches
            "g L" 'magit-log-buffer-file)
          (which-key-add-key-based-replacements "SPC g" "Magit"))
  :config (progn
            (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)))

(use-package evil-magit
  :ensure t
  :after (magit))

(provide 'init-magit)
