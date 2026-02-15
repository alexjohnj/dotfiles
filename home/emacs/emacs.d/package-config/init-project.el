;;; init-project.el --- Project Stuff -*- lexical-binding: t; -*-

;;; Code:

(use-package project
  :ensure nil
  :init
  (which-key-add-key-based-replacements "SPC p" "Project")
  :general
  (alex/leader-def
    "p f" #'project-find-file
    "p d" #'project-find-dir
    "p D" #'project-dired
    "p Y" #'alex-copy-relative-buffer-path

    "p b" #'consult-project-buffer
    "p B" #'project-list-buffers
    "p k" #'project-kill-buffers

    "p p" #'project-switch-project
    "p -" #'project-forget-project

    "p &" #'project-async-shell-command
    "p !" #'project-shell-command
    "p c" #'project-compile

    "p g" #'alex-project-find-regexp
    "p r" #'project-query-replace-regexp)
  :config
  (setopt project-switch-commands #'magit-project-status))

(defun alex--project-root-or-default ()
  "Return the project root or `default-directory` if not in a project."
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun alex-project-find-regexp ()
  "Search for a regular expression using ripgrep if installed."
  (interactive)
  (if alex/rg-available
      (consult-ripgrep (alex--project-root-or-default))
    (consult-grep (alex--project-root-or-default))))

(defun alex-copy-relative-buffer-path ()
  "Copy the path to the file the current buffer is visiting, relative to the project root.If not in a project, copies the absolute path."
  (interactive)
  (if-let* ((file-name buffer-file-name)
            (relative-path (file-relative-name file-name
                                               (alex--project-root-or-default))))
      (progn
        (kill-new relative-path)
        (message relative-path))
    (error "Buffer is not visiting a file")))

(provide 'init-project)
