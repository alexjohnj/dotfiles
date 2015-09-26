;;; init-flycheck -- Initialise Flycheck Module
(use-package flycheck
  :ensure t
  :config (progn
            (add-hook 'after-init-hook #'global-flycheck-mode)
            (setq flycheck-indication-mode nil)))

(use-package flycheck-pos-tip
  :ensure t
  :config (progn
            (eval-after-load 'flycheck
              '(custom-set-variables
                '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
