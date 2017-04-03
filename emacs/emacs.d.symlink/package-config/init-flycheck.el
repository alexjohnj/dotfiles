;;; init-flycheck -- Initialise Flycheck Module
;; Code:

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (progn
          (add-hook 'after-init-hook #'global-flycheck-mode))
  :config (progn
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
            (which-key-add-key-based-replacements "SPC e" "Flycheck")

            (flycheck-define-checker proselint
              "A linter for prose."
              :command ("proselint" source-inplace)
              :error-patterns
              ((warning line-start (file-name) ":" line ":" column ": "
                        (id (one-or-more (not (any " "))))
                        (message) line-end))
              :modes (text-mode markdown-mode gfm-mode mu4e-compose-mode org-mode latex-mode))

            ;; From https://github.com/tasuwo/.emacs.d
            (flycheck-define-command-checker 'mlint
              "A linter for MATLAB"
              :command `("mlint" source)
              :error-patterns
              '((warning line-start "L " line " (C " (1+ digit) "): " (message) line-end)
                (warning line-start "L " line " (C " (1+ digit) "-" (1+ digit) "): " (message) line-end))
              :modes '(matlab-mode))

            (add-to-list 'flycheck-checkers 'proselint)
            (add-to-list 'flycheck-checkers 'mlint)))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :commands (flycheck-pos-tip-error-messages)
  :config (progn
            (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
