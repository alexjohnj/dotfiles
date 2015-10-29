;;; init-flycheck -- Initialise Flycheck Module
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (progn
          (setq flycheck-indication-mode 'left-fringe
                flycheck-display-errors-delay 0.5)
          (evil-leader/set-key
            "ec" 'flycheck-buffer
            "en" 'flycheck-next-error
            "eN" 'flycheck-previous-error
            "e1" 'flycheck-first-error
            "eC" 'flycheck-clear
            "el" 'flycheck-list-errors
            "ev" 'flycheck-verify-setup
            "et" 'flycheck-mode))
  :config (progn
            (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :init (progn
          (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
