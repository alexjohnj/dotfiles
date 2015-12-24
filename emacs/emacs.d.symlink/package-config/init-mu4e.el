;;; init-mu4e --- mu4e email client configuration
(use-package mu4e
  :commands (mu4e)
  :config (progn
          (setq mu4e-maildir (expand-file-name "~/.mail")
                mu4e-sent-folder "/personal/Sent Items"
                mu4e-drafts-folder "/personal/Drafts"
                mu4e-trash-folder (lambda (msg) (alex/mu4e-get-folder msg 'alex/mu4e-trash-folder))
                mu4e-refile-folder (lambda (msg) (alex/mu4e-get-folder msg 'alex/mu4e-refile-folder))
                mu4e-attachment-dir "~/Downloads"
                mu4e-view-show-images t
                message-kill-buffer-on-exit t
                mu4e-change-filenames-when-moving t ; mbsync needs this to avoid UID errors
                mu4e-html2text-command "w3m -T text/html")

          (setq message-send-mail-function 'message-send-mail-with-sendmail
                sendmail-program "/usr/local/bin/msmtp"
                message-sendmail-extra-arguments '("--read-envelope-from")
                message-sendmail-f-is-evil t)

          (setq user-mail-address "alex@alexj.org"
                user-full-name "Alex Jackson")

          (defvar alex/mu4e-account-alist
            '(("personal"
               (mu4e-sent-folder "/personal/Sent Items")
               (mu4e-drafts-folder "/personal/Drafts")
               (alex/mu4e-trash-folder "/personal/Trash")
               (alex/mu4e-refile-folder "/personal/Archive")
               (user-mail-address "alex@alexj.org")
              ("uofa"
               (mu4e-sent-folder "/uofa/[Gmail].Sent Mail")
               (mu4e-drafts-folder "/uofa/[Gmail].Drafts")
               (alex/mu4e-trash-folder "/uofa/[Gmail].Trash")
               (alex/mu4e-refile-folder "/uofa/[Gmail].All Mail")
               (user-mail-address "padillaj@ualberta.ca")
               (mu4e-sent-messages 'delete)))))

          (defun alex/mu4e-get-folder (msg folder)
            "Find the symbol FOLDER in alex/mu4e-account-alist for the account MSG belongs to."
            (let* ((account
                    (let ((maildir (mu4e-message-field msg :maildir)))
                      (string-match "/\\(.*?\\)/" maildir)
                      (match-string 1 maildir)))
                   (account-vars (cdr (assoc account alex/mu4e-account-alist))))
              (if account-vars
                  (cadr (assoc folder account-vars)))))

          (defun alex/mu4e-set-account ()
            "Set the account for composing a message."
            (let* ((account
                    (if mu4e-compose-parent-message
                        (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                          (string-match "/\\(.*?\\)/" maildir)
                          (match-string 1 maildir))
                      (completing-read (format "Compose with account: (%s) "
                                               (mapconcat #'(lambda (var) (car var))
                                                          alex/mu4e-account-alist "/"))
                                       (mapcar #'(lambda (var) (car var)) alex/mu4e-account-alist)
                                       nil t nil nil (caar alex/mu4e-account-alist))))
                   (account-vars (cdr (assoc account alex/mu4e-account-alist))))
              (if account-vars
                  (mapc #'(lambda (var)
                            (set (car var) (cadr var)))
                        account-vars)
                (error "No email account found"))))

          ;; Always sign messages with PGP key
          (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
          ;; Set account when composing messages
          (add-hook 'mu4e-compose-pre-hook 'alex/mu4e-set-account)

          ;; Vimify Key Bindings
          (with-eval-after-load "mu4e"
            (evil-make-overriding-map mu4e-main-mode-map 'normal t)
            (evil-define-key 'normal mu4e-main-mode-map
              "<SPC>" nil
              "j" nil
              "J" 'mu4e~headers-jump-to-maildir
              "/" 'mu4e-headers-search)

            (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
            (evil-define-key 'normal mu4e-headers-mode-map
              "J" 'mu4e~headers-jump-to-maildir
              "j" 'evil-next-line
              "k" 'evil-previous-line)

            (evil-make-overriding-map mu4e-view-mode-map 'normal t)
            (evil-define-key 'normal mu4e-view-mode-map
              "j" 'evil-next-line
              "k" 'evil-previous-line))
          (evil-set-initial-state 'mu4e-mode 'normal)
          (evil-set-initial-state 'mu4e-main-mode 'normal)
          (evil-set-initial-state 'mu4e-headers-mode 'normal)
          (evil-set-initial-state 'mu4e-view-mode 'normal)
          (evil-set-initial-state 'mu4e-compose-mode 'normal)))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
