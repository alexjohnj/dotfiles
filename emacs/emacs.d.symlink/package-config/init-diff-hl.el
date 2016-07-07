;;; init-diff-hl --- Configuration for diff-hl
(use-package diff-hl
  :ensure t
  :commands (turn-on-diff-hl-mode)
  :init (progn
          (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
          (add-hook 'vc-dir-mode 'turn-on-diff-hl-mode))
  :config (progn
            (setq diff-hl-side 'right)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            (unless (display-graphic-p)
              (diff-hl-margin-mode))
            (diff-hl-flydiff-mode)))

(provide 'init-diff-hl)
;;; init-diff-hl.el ends here
