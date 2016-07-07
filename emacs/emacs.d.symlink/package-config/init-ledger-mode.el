(use-package ledger-mode
  :ensure t
  :mode ("\\.journal\\'" . ledger-mode)
  :config (progn
          (if (eq system-type 'darwin) (setq ledger-binary-path "/usr/local/bin/ledger"))
          (setq ledger-post-amount-alignment-column 60
                ledger-post-auto-adjust-amounts t
                ledger-highlight-xact-under-point nil
                ledger-use-iso-dates t
                ledger-reconcile-default-commodity "GBP")
          (evil-leader/set-key-for-mode 'ledger-mode
            "mn" 'ledger-navigate-next-xact-or-directive
            "mN" 'ledger-navigate-prev-xact-or-directive
            "mp" 'ledger-display-balance-at-point
            "ma" 'ledger-add-transaction
            "md" 'ledger-delete-current-transaction
            "me" 'ledger-toggle-current-transaction
            "mc" 'ledger-toggle-current
            "my" 'ledger-copy-transaction-at-point
            "ml" 'ledger-display-ledger-stats
            "mq" 'ledger-post-align-xact
            "mr" 'ledger-reconcile
            "ms" 'ledger-sort-region
            "mt" 'ledger-insert-effective-date
            "mu" 'ledger-schedule-upcoming
            "mor" 'ledger-report
            "mos" 'ledger-report-save
            "mi" 'alex/ledger-attach-receipt
            "mI" 'alex/ledger-open-attached-receipt)

          (evil-define-key 'normal ledger-reconcile-mode-map
            "l" 'ledger-reconcile-refresh
            "a" 'ledger-reconcile-add
            "p" 'ledger-display-balance
            "d" 'ledger-reconcile-delete
            "r" 'ledger-reconcile
            "j" 'next-line
            "k" 'previous-line
            "q" 'ledger-reconcile-quit
            "s" 'ledger-reconcile-save
            "c" 'ledger-reconcile-change-target
            "t" 'ledger-reconcile-toggle
            "RET" 'ledger-reconcile-visit)

          ;; Custom functions for attaching receipts to transactions
          (defvar alex/ledger-receipt-folder "~/finance/receipts")
          (defun alex/ledger-get-xact-date ()
            "Read the effective date (before =) of a
transaction. Returns the date as a time value."
            (save-excursion
              (ledger-navigate-beginning-of-xact)
              (re-search-forward ledger-iso-date-regexp)
              (encode-time 0 0 0 (string-to-number (match-string 4))
                           (string-to-number (match-string 3))
                           (string-to-number (match-string 2)))))

          (defun alex/ledger-construct-receipt-path (date hash &optional ext)
            "Construct a path to a receipt file. DATE is a time value. HASH is a
string. EXT is the file extension (with dot) and defaults to .pdf"
            (unless ext (setq ext ".pdf"))
            (concat (file-name-as-directory alex/ledger-receipt-folder)
                    (file-name-as-directory (format-time-string "%Y" date))
                    (file-name-as-directory (format-time-string "%m" date))
                    hash
                    ext))

          (defun alex/ledger-attach-receipt ()
            "Prompt for a receipt file, calculate its hash and move the file to
alex/ledger-receipt-folder, renaming it to its hash. Inserts the new file name
as a comment to the transaction."
            (interactive)
            (let* ((fname (read-file-name "Receipt File Name:"))
                   (fhash (with-temp-buffer
                            (insert-file-contents fname)
                            (secure-hash 'sha1 (current-buffer))))
                   (xdate (alex/ledger-get-xact-date))
                   (newpath (alex/ledger-construct-receipt-path xdate fhash (file-name-extension fname t))))
              (mkdir (file-name-directory newpath) t)
              (copy-file fname newpath)
              (save-excursion
                (ledger-navigate-beginning-of-xact)
                (end-of-line)
                (insert "\n; Receipt: " fhash (file-name-extension fname t))
                (indent-line-to ledger-post-account-alignment-column))))

          (defun alex/ledger-open-attached-receipt ()
            "Open the receipt file specified by the Receipt tag in a new buffer."
            (interactive)
            (save-excursion
              (ledger-navigate-beginning-of-xact)
              (let* ((xact-date (alex/ledger-get-xact-date))
                     (xact-end (save-excursion (ledger-navigate-end-of-xact)))
                     (receipt-pos (re-search-forward "; Receipt: \\(.*\\)" xact-end t nil))
                     (receipt-hash-fname (match-string 1)))
                (when (not receipt-pos) (error "No receipt found for current transaction"))
                (find-file (alex/ledger-construct-receipt-path xact-date
                                                               (file-name-base receipt-hash-fname)
                                                               (file-name-extension receipt-hash-fname t))))))
          ))

(use-package flycheck-ledger
  :ensure t
  :after (ledger flycheck))

(provide 'init-ledger-mode)
