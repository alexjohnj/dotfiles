(use-package magit
  :commands (magit-dispatch
             magit-status
             magit-file-dispatch
             alex/copy-branch-name)
  :general
  (alex/leader-def
    "g g" #'magit-dispatch
    "g s" #'magit-status
    "g f" #'magit-file-dispatch)
  :init
  (which-key-add-key-based-replacements "SPC g" "Magit")
  :config
  (setq magit-section-initial-visibility-alist
        '(([unpushed status] . show)
          ([unstaged status] . show)
          ([untracked status] . show)))
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

  (defun alex/copy-branch-name (prefix)
    "Copy the name of the branch at point or the current branch's
name if there is no branch at point. With a prefix argument,
always copies the name of the current branch."
    (interactive "P")
    (let ((branch-name (if prefix
                           (magit-get-current-branch)
                         (or (magit-branch-at-point) (magit-get-current-branch)))))
      (if branch-name
          (progn (kill-new branch-name)
                 (message "%s" branch-name))
        (user-error "No branch at point")))))

(use-package git-timemachine
  :commands (git-timemachine)
  :general
  (alex/leader-def
    "g t" #'git-timemachine)
  :config
  (evil-set-initial-state 'git-timemachine-mode 'emacs))

(provide 'init-magit)
