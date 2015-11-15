;;; init-latex --- LaTeX Editing Modes
;;; Commentary:
;;; Code:
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
    (setq-default TeX-engine 'xelatex)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-electric-math (cons "\\(" . "\\)")
          LaTeX-electric-math-left-right-brace t
          TeX-electric-sub-and-superscript t
          LaTeX-fill-break-at-separators '(\\\( \\\[))))

(use-package company-auctex
  :ensure t
  :defer t
  :init
  (progn
    (push 'company-auctex-labels company-backends)
    (push 'company-auctex-bibs company-backends)
    (push '(company-auctex-macros
            company-auctex-symbols
            company-auctex-environments) company-backends)))

(provide 'init-latex)
;;; init-latex.el ends here
