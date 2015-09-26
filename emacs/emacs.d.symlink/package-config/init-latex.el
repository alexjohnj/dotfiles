;;; latex --- LaTeX Editing Modes
;;; Commentary:
;;; Code:
(use-package auctex
  :ensure t
  :mode ("\\.tex" . latex-mode)
  :config
  (progn
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-electric-math (cons "\\(" . "\\)")
          LaTeX-electric-math-left-right-brace t
          TeX-electric-sub-and-superscript t)))

(use-package company-auctex
  :ensure t
  :config
  (progn
    (company-auctex-init)))
  
(provide 'init-latex)
;;; init-latex.el ends here
