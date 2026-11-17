;;; init-project.el --- Project Stuff -*- lexical-binding: t; -*-

;;; Code:

(defvar alex--project-pick-worktree nil
  "Non-nil when the project prompt was exited with `alex-project-exit-to-worktree'.")

(defvar-keymap alex-project-prompt-map
  "C-<return>" #'alex-project-exit-to-worktree)

(defun alex-project-exit-to-worktree ()
  "Select the current project candidate and then prompt for one of its worktrees."
  (interactive)
  (setq alex--project-pick-worktree t)
  (vertico-exit))

(defun alex--project-linked-worktree-p (dir)
  "Return non-nil if DIR is a linked (non-main) git worktree."
  (and (not (file-remote-p dir))
       (let ((dot-git (expand-file-name ".git" dir)))
         (and (file-regular-p dot-git)
              (with-temp-buffer
                (insert-file-contents dot-git)
                (re-search-forward "^gitdir: .*/worktrees/[^/]+/?$" nil t))))))

(defun alex--project-read-worktree (dir)
  "Prompt for a worktree of the repository at DIR and return its path."
  (require 'magit)
  (let* ((default-directory dir)
         (choices (mapcar (pcase-lambda (`(,path ,_commit ,branch . ,_))
                            (cons (format "%-40s %s"
                                          (or branch "(detached)")
                                          (abbreviate-file-name path))
                                  (file-name-as-directory path)))
                          (magit-list-worktrees))))
    (if (length= choices 1)
        (cdar choices)
      (alist-get (completing-read "Select worktree: " choices nil t)
                 choices nil nil #'equal))))

(defun alex-project-prompt-project-dir (&optional prompt predicate require-known)
  "Like `project-prompt-project-dir' but hide linked git worktrees.
Press \\<alex-project-prompt-map>\\[alex-project-exit-to-worktree] to pick a worktree of the selected project.
PROMPT, PREDICATE and REQUIRE-KNOWN are passed to `project-prompt-project-dir'."
  (setq alex--project-pick-worktree nil)
  (let ((dir (minibuffer-with-setup-hook
                 (lambda ()
                   (use-local-map (make-composed-keymap alex-project-prompt-map
                                                        (current-local-map))))
               (project-prompt-project-dir
                prompt
                ;; Called with either a root string or a completion table entry.
                (lambda (choice)
                  (and (not (alex--project-linked-worktree-p
                             (if (consp choice) (car choice) choice)))
                       (or (null predicate) (funcall predicate choice))))
                require-known))))
    (if alex--project-pick-worktree
        (alex--project-read-worktree dir)
      dir)))

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
    "p r" #'project-query-replace-regexp

    "p S" #'alex-project-save-buffers)
  :config
  (setopt project-switch-commands #'magit-project-status
          project-prompter #'alex-project-prompt-project-dir))

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

(defun alex-project-save-buffers ()
  "Save all file-visiting buffers in the current project."
  (interactive)
  (let* ((proj (project-current t))
         (root (project-root proj))
         (saved 0))
    (dolist (buf (project-buffers proj))
      (when (and (buffer-file-name buf)
                 (buffer-modified-p buf))
        (with-current-buffer buf
          (save-buffer)
          (cl-incf saved))))
    (message "Saved %d buffer(s) in %s" saved root)))

(provide 'init-project)
