;;; init-magit.el --- Magit configuration -*- lexical-binding: t -*-

(use-package magit
  :commands (alex/copy-branch-name alex/copy-revision-range)
  :general
  (alex/leader-def
    "g g" #'magit-dispatch
    "g s" #'magit-status
    "g f" #'magit-file-dispatch)
  :init
  (which-key-add-key-based-replacements "SPC g" "Magit")
  :config
  (put 'magit-diff-edit-hunk-commit 'disabled nil)

  (setopt magit-section-initial-visibility-alist
          '(([unpushed status] . show)
            ([unstaged status] . show)
            ([untracked status] . show)))

  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (setopt magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

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
        (user-error "No branch at point"))))

  (defun alex/copy-revision-range (inclusive)
    "Copy the range of commits selected in the region as OLDEST..NEWEST.
With a prefix argument, copy OLDEST^..NEWEST so the range includes
the oldest selected commit."
    (interactive "P")
    (let ((commits (magit-region-values 'commit t)))
      (unless commits
        (user-error "No commit range selected"))
      (let ((range (format (if inclusive "%s^..%s" "%s..%s")
                           (magit-rev-abbrev (car (last commits)))
                           (magit-rev-abbrev (car commits)))))
        (kill-new range)
        (message "%s" range)))))

(use-package magit-delta
  :after (magit)
  :hook (magit-mode . magit-delta-mode)
  :diminish)

(provide 'init-magit)
