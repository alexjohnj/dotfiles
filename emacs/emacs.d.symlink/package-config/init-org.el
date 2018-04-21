;; init-org.el --- Configuration for org-mode and related packages.
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config (progn
            (add-hook 'org-mode-hook 'auto-fill-mode)
            (add-hook 'org-mode-hook (lambda ()
                                       (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))
            (setq org-startup-indented t)
            (evil-leader/set-key-for-mode 'org-mode
              "m E" 'org-export-dispatch
              "m p l" 'org-toggle-latex-fragment
              "m p i" 'org-toggle-inline-images
              "m p u" 'org-toggle-pretty-entities
              "m n s" 'org-narrow-to-subtree
              "m n b" 'org-narrow-to-block
              "m n w" 'widen
              "m n i" 'org-tree-to-indirect-buffer)))

(use-package evil-org
  :after org
  :ensure t)

(provide 'init-org)
;;; init-org.el ends here
