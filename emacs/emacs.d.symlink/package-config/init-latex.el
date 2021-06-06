;;; init-latex --- LaTeX Editing Modes
;;; Commentary:
;;; Code:
(use-package tex-site
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config (progn
            (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
            (add-hook 'TeX-mode-hook 'turn-on-auto-fill)
            (setq-default TeX-engine 'lualatex)
            (setq-default TeX-master nil)
            (setq TeX-auto-save t
                  TeX-parse-self t
                  TeX-electric-math (cons "\\(" "\\)")
                  LaTeX-electric-math-left-right-brace t
                  LaTeX-electric-left-right-brace t
                  TeX-electric-sub-and-superscript t
                  LaTeX-fill-break-at-separators '(\\\( \\\[))

            (alex/leader-local-def latex-mode-map
              "p e" #'preview-environment
              "p d" #'preview-document
              "p b" #'preview-buffer
              "p r" #'preview-region
              "p s" #'preview-section
              "s"   #'LaTeX-section
              "e"   #'LaTeX-environment
              "c"   #'LaTeX-close-environment
              "="   #'TeX-master-file-ask)))

(use-package reftex
  :commands (turn-on-reftex)
  :init (progn
          (add-hook 'LaTeX-mode-hook 'turn-on-reftex))
  :config (progn
            (setq reftex-plug-into-AUCTeX t)

            (general-def 'normal reftex-toc-mode-map
              "j" #'reftex-toc-next
              "k" #'reftex-toc-previous
              "Q" #'reftex-toc-quit-and-kill
              "%" #'reftex-toc-rename-label)

            (alex/leader-local-def latex-mode-map
              "r r" #'reftex-reference
              "r l" #'reftex-label
              "r t" #'reftex-toc
              "r v" #'reftex-view-crossref)))

(use-package company-auctex
  :after (latex)
  :config (progn
            (company-auctex-init)))

(provide 'init-latex)
;;; init-latex.el ends here
