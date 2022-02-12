(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing nil
        web-mode-enable-engine-detection t
        web-mode-enable-current-element-highlight t)

  (add-hook 'web-mode-hook #'alex/web-mode--configure-eex-pairing)
  (add-hook 'web-mode-hook #'aggressive-indent-mode))

(provide 'init-web-mode)

(defun alex/web-mode--configure-eex-pairing ()
  (sp-with-modes '(web-mode)
    (sp-local-pair "<% " " %>" :insert "C-b %")
    (sp-local-pair "<%= " " %>" :insert "C-b =")
    (sp-local-pair "<%# " " %>" :insert "C-b #")))
