;;; init-org.el --- Configuration for org-mode and related packages.
;;; Commentary:
;;; Code:

(use-package org-plus-contrib
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init (progn
          (setq org-startup-indented t
                org-modules '(org-drill))
          (setq org-latex-pdf-process
                '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")))
  :config (progn
            (org-load-modules-maybe)))

(use-package org-crypt
  :config (setq org-crypt-key "B6CA4B58"))

(use-package evil-org
  :ensure t)

(use-package org-journal
  :ensure t
  :init (progn
          (evil-leader/set-key
            "+" 'org-journal-new-entry
            "=" '(lambda () (interactive) (org-journal-new-entry t)))
          (which-key-add-key-based-replacements
            "SPC +" "Add entry to journal"
            "SPC =" "View today's journal")
          (setq org-journal-dir (expand-file-name "~/Dropbox/Documents/Journal/")
                org-journal-file-format "%Y-%m-%d.org"
                org-journal-date-format "%A, %d-%m-%Y"
                org-journal-enable-encryption t)
          (evil-leader/set-key-for-mode 'calendar-mode
            "m j j" 'org-journal-read-entry
            "m j i" 'org-journal-new-date-entry
            "m j [" 'org-journal-previous-entry
            "m j ]" 'org-journal-next-entry
            "m j f f" 'org-journal-search-forever
            "m j f m" 'org-journal-search-calendar-month
            "m j f w" 'org-journal-search-calender-week
            "m j f y" 'org-journal-search-calendar-year)
          (evil-leader/set-key-for-mode 'org-journal-mode
            "m j [" 'org-journal-open-previous-entry
            "m j ]" 'org-journal-open-next-entry)))

(provide 'init-org)
;;; init-org.el ends here
