
;;; init-tramp.el --- TRAMP optimizations -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Optimizations for TRAMP based on https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
;;
;;; Code:

(use-package tramp
  :ensure nil
  :config
  (setopt remote-file-name-inhibit-locks t
          tramp-use-scp-direct-remote-copying t
          remote-file-name-inhibit-auto-save-visited t
          tramp-verbose 5
          tramp-use-connection-share nil
          tramp-copy-size-limit (* 1024 1024) ;; 1MB
          tramp-verbose 2)

  ;; Direct Async Processes
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :machine ".*")
   'remote-direct-async-process)

  (setopt magit-tramp-pipe-stty-settings 'pty)

  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(provide 'init-tramp)
;;; init-tramp.el ends here
