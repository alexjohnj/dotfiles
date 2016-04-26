;;; init-flycheck -- Initialise Flycheck Module
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (progn
          (setq flycheck-indication-mode 'left-fringe
                flycheck-display-errors-delay 0.5)
          (evil-leader/set-key
            "ec" 'flycheck-buffer
            "en" 'flycheck-next-error
            "eN" 'flycheck-previous-error
            "e1" 'flycheck-first-error
            "eC" 'flycheck-clear
            "el" 'flycheck-list-errors
            "ev" 'flycheck-verify-setup
            "et" 'flycheck-mode)
          (which-key-add-key-based-replacements "SPC e" "Flycheck"))
  :config (progn
            (add-hook 'after-init-hook #'global-flycheck-mode)
            (flycheck-define-checker proselint
              "A linter for prose."
              :command ("proselint" source-inplace)
              :error-patterns
              ((warning line-start (file-name) ":" line ":" column ": "
                        (id (one-or-more (not (any " "))))
                        (message) line-end))
              :modes (text-mode markdown-mode gfm-mode mu4e-compose-mode))
            (add-to-list 'flycheck-checkers 'proselint)))

(use-package flycheck-pos-tip
  :ensure t
  :commands (flycheck-pos-tip-error-messages)
  :init (progn
          (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
