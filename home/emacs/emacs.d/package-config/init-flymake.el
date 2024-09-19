(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :straight nil ; Ensure the built in flymake package is used.
  :general (alex/leader-def
             "e c" #'flymake-start
             "e n" #'flymake-goto-next-error
             "e p" #'flymake-goto-prev-error
             "e l" #'flymake-show-buffer-diagnostics
             "e t" #'flymake-mode
             "e v" #'flymake-running-backends)
  :init
  (which-key-add-key-based-replacements "SPC e" "Flymake"))

(defun alex/flymake-eslint-enable ()
  (when (and (or (derived-mode-p 'js-base-mode)
                 (derived-mode-p 'typescript-ts-base-mode))
             (executable-find "eslint"))
    (flymake-eslint-enable)))

(use-package flymake-eslint
  :after (flymake)
  :straight '(flymake-eslint :type git :host github :repo "orzechowskid/flymake-eslint")
  :commands (flymake-eslint-enable)
  ;; eglot will override flymake checkers when it starts so start the eslint checker
  ;; after eglot with a custom hook. See: https://github.com/orzechowskid/flymake-eslint/issues/23
  :hook (eglot-managed-mode . alex/flymake-eslint-enable)
  :config
  (setopt flymake-eslint-prefer-json-diagnostics t))

(provide 'init-flymake)
