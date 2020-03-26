;;; init-flycheck -- Initialise Flycheck Module
;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :hook ((after-init . global-flycheck-mode))
  :config
  (setq flycheck-indication-mode 'left-fringe
        flycheck-display-errors-delay 0.5)

  ;; Disable flycheck elisp documentation checker
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

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

  (add-to-list 'flycheck-checkers 'proselint))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
