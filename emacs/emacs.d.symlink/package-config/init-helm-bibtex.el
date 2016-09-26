; See also: init-org with org-ref that links with this
(use-package helm-bibtex
  :ensure t
  :commands (helm-bibtex helm-source-bibtex)
  :after (latex)
  :init (progn
          (evil-leader/set-key
            "r s" 'helm-bibtex))
  :config (progn
            (setq bibtex-completion-bibliography '("~/Dropbox/Research/library.bib")
                  bibtex-completion-library-path '("~/Dropbox/Research/pdfs/")
                  bibtex-completion-notes-path "~/Dropbox/Research/notes.org"
                  bibtex-completion-pdf-open-function (lambda (fpath)
                                                        (call-process "open" nil 0 nil fpath))
                  bibtex-completion-additional-search-fields '(tags keywords)
                  bibtex-completion-cite-default-command "parencite"
                  bibtex-completion-cite-prompt-for-optional-arguments nil
                  bibtex-completion-notes-template-one-file "** TODO (${year}): ${title}\n:PROPERTIES:\n:Custom_ID: ${=key=}\n:AUTHORS: ${author}\n:END:\n[[cite:${=key=}]] [[file:./pdfs/${=key=}.pdf][pdf]]")
            (evil-leader/set-key
              "r U" 'alex/search-unread-papers)
            (eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "C-c ]") 'helm-bibtex))))

(defun alex/search-unread-papers ()
  (interactive)
  (helm :sources '(helm-source-bibtex)
        :full-frame nil
        :input "toread"))

(provide 'init-helm-bibtex)
