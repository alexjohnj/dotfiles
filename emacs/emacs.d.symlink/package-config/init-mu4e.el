;;; init-mu4e --- mu4e email client configuration
(use-package mu4e
  :commands (mu4e)
  :config (progn
          (setq mu4e-maildir (expand-file-name "~/.mail")
                mu4e-sent-folder "/personal/Sent Items"
                mu4e-drafts-folder "/personal/Drafts"
                mu4e-trash-folder "/personal/Trash"
                mu4e-refile-folder "/personal/Archive"
                mu4e-attachment-dir "~/Downloads"
                mu4e-view-show-images t
                message-kill-buffer-on-exit t
                mu4e-change-filenames-when-moving t ; mbsync needs this to avoid UID errors
                mu4e-html2text-command "w3m -T text/html")

          (setq user-mail-address "alex@alexj.org"
                user-full-name "Alex Jackson")

          (setq message-send-mail-function 'smtpmail-send-it
                smtpmail-stream-type 'ssl
                smtpmail-default-smtp-server "mail.messagingengine.com"
                smtpmail-smtp-server "mail.messagingengine.com"
                smtpmail-smtp-service 465)

          ;; Always sign messages with PGP key
          (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

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
