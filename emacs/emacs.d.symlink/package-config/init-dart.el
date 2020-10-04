(use-package dart-mode
  :mode ("\\.dart\\'" . dart-mode))

(use-package lsp-dart
  :after (dart-mode))

(provide 'init-dart)
