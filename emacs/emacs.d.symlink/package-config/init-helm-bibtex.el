; See also: init-org with org-ref that links with this
(use-package helm-bibtex
  :ensure t
  :commands (helm-bibtex helm-source-bibtex)
  :init (progn
          (evil-leader/set-key
            "r s" 'helm-bibtex))
  :config (progn
            (setq bibtex-completion-bibliography '("~/Dropbox/Research/library.bib")
                  bibtex-completion-library-path '("~/Dropbox/Research/pdfs/")
                  bibtex-completion-notes-path "~/Dropbox/Research/notes.org"
                  bibtex-completion-pdf-open-function (lambda (fpath)
                                                        (call-process "open" nil 0 nil fpath))
                  bibtex-completion-additional-search-fields '(tags keywords))))

(provide 'init-helm-bibtex)
