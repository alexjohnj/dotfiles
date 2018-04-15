(use-package beancount
  :ensure nil
  :mode ("\\.beancount\\'" . beancount-mode)
  :config (progn
            (add-hook
             'beancount-mode-hook
             (lambda ()
               (setq-local indent-line-function 'alex/beancount--indent-line)
               (setq-local indent-region-function 'alex/beancount--indent-region)))))

;; Indentation code
(defconst alex/beancount--transaction-regexp
  "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-\\{1\\}\\(txn\\|\\*\\|!\\).*$")

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

(defun alex/beancount-format-file ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (call-process-region (point-min) (point-max) "bean-format" t (current-buffer))
    (goto-line line-no)
    (recenter)))

(provide 'init-beancount-mode)
;;; init-beancount-mode.el ends here
