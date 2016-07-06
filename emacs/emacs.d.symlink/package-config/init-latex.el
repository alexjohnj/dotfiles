;;; init-latex --- LaTeX Editing Modes
;;; Commentary:
;;; Code:
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config (progn
            (message "Hi from auctex")
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
            (evil-leader/set-key-for-mode 'latex-mode
              "m p e" 'preview-environment
              "m p d" 'preview-document
              "m p b" 'preview-buffer
              "m p r" 'preview-region
              "m p s" 'preview-section
              "m s"   'LaTeX-section
              "m e"   'LaTeX-environment
              "m c"   'LaTeX-close-environment
              "m ="   'TeX-master-file-ask)))

(use-package reftex
  :ensure t
  :commands (turn-on-reftex)
  :init (progn
          (add-hook 'LaTeX-mode-hook 'turn-on-reftex))
  :config (progn
            (setq reftex-plug-into-AUCTeX t)
            (evil-define-key 'normal reftex-toc-mode-map
              (kbd "j") 'reftex-toc-next
              (kbd "k") 'reftex-toc-previous
              (kbd "Q") 'reftex-toc-quit-and-kill
              (kbd "%") 'reftex-toc-rename-label)
            (evil-leader/set-key-for-mode 'latex-mode
              "m r c" 'reftex-citation
              "m r r" 'reftex-reference
              "m r l" 'reftex-label
              "m r t" 'reftex-toc
              "m r v" 'reftex-view-crossref)))

(use-package company-auctex
  :ensure t
  :after (latex)
  :config (progn
            (company-auctex-init)))

(provide 'init-latex)
;;; init-latex.el ends here
