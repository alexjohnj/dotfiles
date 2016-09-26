;; init-org.el --- Configuration for org-mode and related packages.
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config (progn
            (add-hook 'org-mode-hook 'auto-fill-mode)
            (setq org-startup-indented t)
            (evil-leader/set-key-for-mode 'org-mode
              "m E" 'org-export-dispatch
              "m p l" 'org-preview-latex-fragment
              "m p i" 'org-toggle-inline-images
              "m p u" 'org-toggle-pretty-entities
              "m n s" 'org-narrow-to-subtree
              "m n b" 'org-narrow-to-block
              "m n w" 'widen
              "m n i" 'org-tree-to-indirect-buffer)))

(use-package evil-org
  :after org
  :ensure t)

(use-package org-ref
  :after (org reftex latex)
  :ensure t
  :init (progn
          (setq org-ref-completion-library 'org-ref-helm-cite))
  :config (progn
            (setq org-ref-notes-directory "~/Dropbox/Research/notes/"
                  org-ref-bibliography-notes "~/Dropbox/Research/notes.org"
                  org-ref-pdf-directory "~/Dropbox/Research/pdfs/"
                  org-ref-default-bibliography '("~/Dropbox/Research/library.bib")
                  org-ref-notes-function  '(lambda (key)
                                             (bibtex-completion-edit-notes (car (org-ref-get-bibtex-key-and-file)))))
            (evil-leader/set-key-for-mode 'bibtex-mode
              "m n" 'org-ref-open-bibtex-notes
              "m o" 'org-ref-open-bibtex-pdf
              "m b" 'org-ref-open-in-browser
              "m c" 'org-ref-clean-bibtex-entry)
            (evil-leader/set-key-for-mode 'latex-mode
              "m r c" 'org-ref-helm-insert-cite-link)))

(use-package doi-utils
  :commands (doi-utils-add-bibtex-entry-from-doi
             doi-utils-add-entry-from-crossref-query
             doi-utils-get-bibtex-entry-pdf)
  :init (progn
          (evil-leader/set-key
            "r a d" 'doi-utils-add-bibtex-entry-from-doi
            "r a c" 'doi-utils-add-entry-from-crossref-query)
          (evil-leader/set-key-for-mode 'bibtex-mode "m g" 'doi-utils-get-bibtex-entry-pdf)))

(use-package org-ref-pdf
  :after org-ref)

(use-package org-ref-url-utils
  :after org-ref)

(provide 'init-org)
;;; init-org.el ends here
