(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-switch-project projectile-project-p projectile-find-file)
  :init (progn
          (evil-leader/set-key
            "pf" 'projectile-find-file
            "pp" 'projectile-switch-project
            "p-" 'alex/projectile-remove-known-project)
          (which-key-add-key-based-replacements "SPC p" "Project"))
  :config (progn
            (setq projectile-switch-project-action 'magit-status
                  projectile-enable-caching t
                  projectile-require-project-root nil)

            (when (executable-find "fd")
              (setq projectile-generic-command "fd . --type f -0"
                    projectile-git-command "fd . --type f -0"))

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
              "p c" 'projectile-compile-project)
            (projectile-mode)))

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

(provide 'init-projectile)
