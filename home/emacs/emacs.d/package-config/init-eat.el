;;; init-eat.el --- Eat terminal emulator -*- lexical-binding: t -*-

(use-package eat
  :after evil
  :commands (eat)
  :config
  (defun alex/eat-new-session ()
    "Run eat in a new session."
    (interactive)
    (eat nil t))

  (setopt eat-kill-buffer-on-exit t)

  (add-to-list 'evil-insert-state-modes 'eat-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (alex/leader-def
    "t t" #'eat
    "t n" #'alex/eat-new-session))

(provide 'init-eat)
