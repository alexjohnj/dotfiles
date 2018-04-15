;;; init-mu4e --- mu4e email client configuration
(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :config (progn
            (require 'mu4e-contrib)
            (setq mu4e-maildir (expand-file-name "~/.mail")
                  mu4e-attachment-dir "~/Downloads"
                  mu4e-view-show-images t
                  message-kill-buffer-on-exit t
                  mu4e-change-filenames-when-moving t ; mbsync needs this to avoid UID errors
                  mu4e-completing-read-function 'ivy-completing-read
                  mu4e-html2text-command 'mu4e-shr2text
                  shr-color-visible-luminance-min  80
                  mu4e-context-policy 'pick-first
                  mu4e-compose-context-policy 'ask)

            (setq message-send-mail-function 'message-send-mail-with-sendmail
                  sendmail-program "/usr/local/bin/msmtp"
                  message-sendmail-extra-arguments '("--read-envelope-from")
                  message-sendmail-f-is-evil t)

            (setq user-mail-address "alex@alexj.org"
                  user-full-name "Alex Jackson")

            (defun alex/mu4e-match-account (msg account)
              (when msg
                (let ((msg-account
                       (let ((maildir (mu4e-message-field msg :maildir)))
                         (string-match "/\\(.*?\\)/" maildir)
                         (match-string 1 maildir))))
                  (string= msg-account account))))

            (setq mu4e-contexts
                  `(,(make-mu4e-context
                      :name "Personal"
                      :match-func (lambda (msg) (alex/mu4e-match-account msg "personal"))
                      :vars '((user-mail-address . "alex@alexj.org")
                              (user-full-name . "Alex Jackson")
                              (mu4e-sent-folder . "/personal/Sent Items")
                              (mu4e-drafts-folder . "/personal/Drafts")
                              (mu4e-trash-folder . "/personal/Trash")
                              (mu4e-refile-folder . "/personal/Archive")
                              (mu4e-sent-messages-behaviour . 'sent)))
                    ,(make-mu4e-context
                      :name "Geography AS"
                      :match-func (lambda (msg) (alex/mu4e-match-account msg "geography"))
                      :vars '((user-mail-address . "alex@geographyas.info")
                              (user-full-name . "Alex Jackson")
                              (mu4e-sent-folder . "/geography/[Gmail].Sent Mail")
                              (mu4e-drafts-folder . "/geography/[Gmail].Drafts")
                              (mu4e-trash-folder . "/geography/[Gmail].Trash")
                              (mu4e-refile-folder . "/geography/[Gmail].All Mail")
                              (mu4e-sent-messsages-behaviour . 'delete)))))

            (add-to-list 'mu4e-bookmarks
                         '("maildir:/personal/INBOX OR maildir:/geography/INBOX" "Unified Inbox" ?i))

            ;; Prompt to sign messages with PGP key
            (add-hook 'message-send-hook
                      '(lambda ()
                         (when (yes-or-no-p "Sign message?")
                           (mml-secure-message-sign-pgpmime))))

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
                "k" 'evil-previous-line
                "oo" 'mu4e-view-go-to-url
                "oa" 'mu4e-view-open-attachment))
            (evil-set-initial-state 'mu4e-mode 'normal)
            (evil-set-initial-state 'mu4e-main-mode 'normal)
            (evil-set-initial-state 'mu4e-headers-mode 'normal)
            (evil-set-initial-state 'mu4e-view-mode 'normal)
            (evil-set-initial-state 'mu4e-compose-mode 'normal)))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
