(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project
             projectile-project-p
             projectile-find-file)
  :init
  (evil-leader/set-key
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "p-" 'alex/projectile-remove-known-project)
  (which-key-add-key-based-replacements "SPC p" "Project")
  :config
  (setq projectile-switch-project-action #'alex/projectile-switch-project
        projectile-enable-caching t
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-completion-system 'ivy)

  (when (executable-find "fd")
    (setq projectile-generic-command "fd . -0 -H -E .git --color=never --type file --type symlink --follow"
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          projectile-indexing-method 'alien))

  (evil-leader/set-key
    "pb" 'projectile-switch-to-buffer
    "p C-f" 'alex/projectile-invalidate-and-search
    "pq" 'projectile-switch-open-project
    "pd" 'projectile-find-dir
    "pk" 'projectile-kill-buffers
    "pD" 'projectile-dired
    "pr" 'projectile-replace
    "psg" 'projectile-grep
    "pss" 'projectile-ag
    "pS" 'projectile-save-project-buffers
    "p c" 'projectile-compile-project
    "p t" 'projectile-find-test-file)
  (projectile-mode))

(defun alex/projectile-remove-known-project ()
  "Removes the current project if you're in one, otherwise prompts for a project to remove."
  (interactive)
  (if (projectile-project-p)
      (projectile-remove-current-project-from-known-projects)
    (call-interactively 'projectile-remove-known-project)))

(defun alex/projectile-invalidate-and-search ()
  (interactive)
  (projectile-invalidate-cache nil)
  (projectile-find-file))

(defun alex/projectile-switch-project ()
  (require 'magit)
  (if (magit-git-repo-p default-directory)
      (magit-status)
    (projectile-dired)))

(provide 'init-projectile)
