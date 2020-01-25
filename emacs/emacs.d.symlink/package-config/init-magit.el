(use-package magit
  :ensure t
  :commands (magit-dispatch-popup magit-status magit-blame-popup magit-log-popup)
  :init (progn
          (alex/evil-leader--prefix "g"
                                    "g" 'magit-dispatch-popup
                                    "s" 'magit-status
                                    "b" 'magit-blame-popup
                                    "l" 'magit-log-popup)
          (which-key-add-key-based-replacements "SPC g" "Magit"))
  :config (progn
            (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
            (magit-define-popup-switch
              'magit-log-popup
              ?1 "First parent" "--first-parent")))

(use-package evil-magit
  :ensure t
  :after (magit))

(provide 'init-magit)
