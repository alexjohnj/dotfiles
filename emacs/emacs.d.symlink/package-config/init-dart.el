(use-package dart-mode
  :mode ("\\.dart\\'" . dart-mode)
  :init
  (after! projectile
    (projectile-register-project-type 'dart '("pubspec.yaml"))
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")))

(use-package lsp-dart
  :after (dart-mode))

(provide 'init-dart)
