;; init-org.el --- Configuration for org-mode and related packages.
;;; Commentary:
;;; Code:

(use-package org
  :straight nil ;; Prefer the built-in org package
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-startup-indented t
        org-list-indent-offset 1
        org-goto-interface 'outline-path-completion
        org-goto-max-level 10
        org-outline-path-complete-in-steps nil
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0)

  (add-hook 'org-mode-hook 'auto-fill-mode)

  (general-def 'normal org-mode-map "TAB" #'org-cyc)
  (alex/leader-local-def org-mode-map
    "E" #'org-export-dispatch
    "p l" #'org-toggle-latex-fragment
    "p i" #'org-toggle-inline-images
    "p u" #'org-toggle-pretty-entities
    "n s" #'org-narrow-to-subtree
    "n b" #'org-narrow-to-block
    "n w" #'widen
    "n i" #'org-tree-to-indirect-buffer
    "t" #'org-todo
    "c i" #'org-clock-in
    "c o" #'org-clock-out
    "c C" #'org-clock-cancel
    "c g" #'org-clock-goto))

(provide 'init-org)
;;; init-org.el ends here
