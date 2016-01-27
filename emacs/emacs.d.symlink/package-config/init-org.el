;;; init-org.el --- Configuration for org-mode and related packages.
;;; Commentary:
;;; Code:

(use-package org-plus-contrib
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init (progn
          (setq org-startup-indented t
                org-modules '(org-drill))
          (evil-leader/set-key-for-mode 'org-mode
            "m E" 'org-export-dispatch
            "m p l" 'org-preview-latex-fragment
            "m p i" 'org-toggle-inline-images))
  :config (progn
            (org-load-modules-maybe)))

(use-package ox-latex
  :init (progn
          (setq org-latex-pdf-process
                '("latexmk -outdir=auto -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
                org-latex-listings 'minted))
  :config (progn
            (add-to-list 'org-latex-packages-alist '("" "minted"))
            (add-to-list 'org-latex-classes
                         '("alex-report"
                           "\\documentclass{article}
                           [NO-DEFAULT-PACKAGES]
                           \\usepackage{fontspec}
                           \\usepackage{polyglossia}
                           \\setdefaultlanguage[variant=british]{english}
                           \\setmonofont[Scale=MatchLowercase]{Consolas}
                           \\usepackage[a4paper,top=31mm,bottom=31mm]{geometry}
                           \\usepackage{microtype}
                           \\usepackage{setspace}
                           \\onehalfspacing
                           \\usepackage[outputdir=auto/]{minted}
                           \\usemintedstyle{bw}
                           \\usepackage{gensymb}
                           \\usepackage{amsmath}
                           \\usepackage{amssymb}
                           \\numberwithin{equation}{section}
                           \\usepackage{graphicx}
                           \\usepackage{float}
                           \\usepackage{caption}
                           \\usepackage{subcaption}
                           \\usepackage{booktabs}
                           \\usepackage{physics}
                           \\usepackage{xfrac}
                           \\usepackage{siunitx}
                           \\sisetup{detect-all}
                           [PACKAGES]
                           \\usepackage{hyperref}
                           \\usepackage{cleveref}
                           \\crefname{figure}{Figure}{Figures}
                           \\Crefname{figure}{Figure}{Figures}
                           [EXTRA]"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

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

(use-package org-research
  :init (progn
          (setq org-research-root "~/Dropbox/research")
          (evil-leader/set-key-for-mode 'org-research-mode
            "m r o" 'org-research-open-paper
            "m r a" 'org-research-add-reference)))

(provide 'init-org)
;;; init-org.el ends here
