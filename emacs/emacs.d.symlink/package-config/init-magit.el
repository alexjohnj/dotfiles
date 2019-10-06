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
            (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)))

(use-package evil-magit
  :ensure t
  :after (magit))

(provide 'init-magit)
