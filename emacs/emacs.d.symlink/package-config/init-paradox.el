;;; init-paradox.el --- Configuration for paradox.el
;;; Commentary: This is mostly just evil key bindings. paradox-github-token is
;;; set in custom.el which is kept out of vc.
(use-package paradox
  :ensure t
  :bind (([f2]  . paradox-list-packages))
  :config (progn
            (setq paradox-automatically-star nil)
            (evil-define-key 'normal paradox-menu-mode-map
              "RET" 'paradox-push-button
              "j" 'paradox-next-entry
              "k" 'paradox-previous-entry
              "h" 'paradox-menu-quick-help
              "l" 'paradox-menu-view-commit-list
              "q" 'paradox-quit-and-close
              "s" 'paradox-menu-mark-star-unstar
              "v" 'paradox-menu-visit-homepage
              "r" 'package-menu-refresh
              "i" 'package-menu-mark-install
              "d" 'package-menu-mark-delete
              "u" 'package-menu-mark-unmark
              "U" 'package-menu-mark-upgrades
              "x" 'package-menu-execute
              "~" 'package-menu-mark-obsolete-for-deletion
              "?" 'package-menu-describe-package
              ;; (kbd "g") nil
              ;; (kbd "g g")'evil-goto-first-line
              )
            (evil-leader/set-key-for-mode 'paradox-menu-mode
              "m s s" 'paradox-sort-by-status
              "m s p" 'paradox-sort-by-package
              "m s *" 'paradox-sort-by-★)))

(provide 'init-paradox)
