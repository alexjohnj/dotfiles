(use-package dart-mode
  :mode ("\\.dart\\'" . dart-mode)
  :init
  (after! projectile
    (projectile-register-project-type 'dart '("pubspec.yaml"))
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")))

(use-package lsp-dart
  :after (dart-mode)
  :config
  ;; TODO: How to tie this to the flutter version for a project?
  (setq lsp-dart-flutter-sdk-dir (expand-file-name "~/.asdf/installs/flutter/1.22.4")
        lsp-dart-dap-flutter-hot-reload-on-save t)
  (evil-leader/set-key-for-mode 'dart-mode
    "m r" #'lsp-dart-dap-flutter-hot-reload
    "m R" #'lsp-dart-dap--flutter-hot-restart))

(provide 'init-dart)
