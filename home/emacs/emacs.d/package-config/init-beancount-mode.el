(use-package beancount
  :straight (beancount :type built-in)
  :mode (("\\.beancount\\'" . beancount-mode))
  :config
  (setq beancount-use-ido nil)
  (add-hook 'beancount-mode-hook #'alex/beancount--configure-buffer-hook))

;; Indentation code
(defconst alex/beancount--transaction-regexp
  "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-\\{1\\}\\(txn\\|\\*\\|!\\|commodity\\).*$")

(defconst alex/beancount--key-value-regexp
  "^\\s-+\\w+:\\s-\\{1\\}.+$")

(defconst alex/beancount--posting-regexp
  "^\\s-+\\w+:.*[0-9].*$")

(defvar alex/beancount--tab-width 4)

(defun alex/beancount--line-is-transaction-p ()
  "Is the current line a transaction?"
  (save-excursion
    (beginning-of-line)
    (looking-at alex/beancount--transaction-regexp)))

(defun alex/beancount--line-is-key-value-p ()
  "Is the current line a key: value statement?"
  (save-excursion
    (beginning-of-line)
    (looking-at alex/beancount--key-value-regexp)))

(defun alex/beancount--line-is-posting ()
  "Is the current line a posting?"
  (save-excursion
    (beginning-of-line)
    (looking-at alex/beancount--posting-regexp)))

(defun alex/beancount--line-needs-indent-p ()
  "Does the current line need indenting?"
  (save-excursion
    (forward-line -1)
    (or (alex/beancount--line-is-posting)
        (alex/beancount--line-is-transaction-p)
        (alex/beancount--line-is-key-value-p))))

(defun alex/beancount--indent-line ()
  "Indent the current line."
  (interactive)
  (when (alex/beancount--line-needs-indent-p)
    (indent-line-to alex/beancount--tab-width)
    (move-to-column (current-indentation))))

(defun alex/beancount--indent-region (start end)
  "Indent the current region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (while (and (not (eobp)) (< (point) end))
      (alex/beancount--indent-line)
      (forward-line 1))))

(defun alex/beancount--configure-buffer-hook ()
  (setq-local indent-line-function 'alex/beancount--indent-line
              indent-region-function 'alex/beancount--indent-region
              org-imenu-depth 3
              apheleia-formatter 'bean-format)
  (smartparens-mode -1) ;; SP is _really_ slow in large files so use electric pair instead.
  (electric-pair-mode t)
  (apheleia-mode t)
  (auto-fill-mode -1))

(provide 'init-beancount-mode)
;;; init-beancount-mode.el ends here
