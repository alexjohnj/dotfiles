(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-switch-project projectile-project-p)
  :init (progn
          (evil-leader/set-key
            "pp" 'projectile-switch-project
            "p-" 'alex/projectile-remove-known-project)
          (which-key-add-key-based-replacements "SPC p" "Project"))
  :config (progn
            (setq projectile-switch-project-action 'projectile-dired)
            (evil-leader/set-key
              "pb" 'projectile-switch-to-buffer
              "pq" 'projectile-switch-open-project
              "pd" 'projectile-find-dir
              "pf" 'projectile-find-file
              "pk" 'projectile-kill-buffers
              "pD" 'projectile-dired
              "pr" 'projectile-replace
              "psg" 'projectile-grep
              "pss" 'projectile-ag
              "pS" 'projectile-save-project-buffers)
            (projectile-global-mode)))

(defun alex/projectile-remove-known-project ()
  "Removes the current project if you're in one, otherwise prompts for a project to remove."
  (interactive)
  (if (projectile-project-p)
      (projectile-remove-current-project-from-known-projects)
    (call-interactively 'projectile-remove-known-project)))

(provide 'init-projectile)
