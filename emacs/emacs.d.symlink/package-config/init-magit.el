(use-package magit
  :ensure t
  :commands (magit-dispatch-popup magit-status)
  :init (progn
          (evil-leader/set-key "g" 'magit-dispatch-popup)
          (which-key-add-key-based-replacements "SPC g" "Magit"))
  :config (progn
            (define-key magit-popup-mode-map (kbd "s")
              '(lambda ()
                 (interactive)
                 (magit-popup-quit)
                 (magit-status)))
            (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
            (magit-define-popup-switch
              'magit-log-popup
              ?1 "First parent" "--first-parent")))

(use-package evil-magit
  :ensure t
  :after (magit))

(provide 'init-magit)
