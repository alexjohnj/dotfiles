(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project
             projectile-project-p
             projectile-find-file
             projectile-add-known-project)
  :init
  (evil-leader/set-key
    "p f" #'projectile-find-file
    "p p" #'projectile-switch-project
    "p -" #'projectile-remove-known-project
    "p +" #'alex/projectile-smart-add-known-projectile)
  (which-key-add-key-based-replacements "SPC p" "Project")

  :config
  (setq projectile-switch-project-action #'alex/projectile-switch-project
        projectile-enable-caching t
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-completion-system 'ivy)

  (when alex/fd-available
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
    "pss" 'rg-project
    "psS" 'rg-dwim-project-dir
    "pS" 'projectile-save-project-buffers
    "p c" 'projectile-compile-project
    "p t" 'projectile-find-test-file)

  (projectile-mode))

(defun alex/projectile-smart-add-known-projectile ()
  "If the current directory looks like a project, adds it as a project. Otherwise asks for a directory"
  (interactive)
  (if (projectile-project-p)
      (projectile-add-known-project (projectile-project-root))
    (projectile-add-known-project (pwd))))

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
