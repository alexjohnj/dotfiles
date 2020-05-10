(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))

(use-package elixir-mode
  :mode (("\\.exs?\\'" . elixir-mode))
  :config
  (add-hook 'elixir-mode-hook 'alex/set-elixir-format-hook))

(defun alex/set-elixir-format-hook ()
  (add-hook 'before-save-hook 'elixir-format nil t))

(provide 'init-elixir)
