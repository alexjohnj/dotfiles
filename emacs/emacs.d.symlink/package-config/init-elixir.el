(use-package elixir-mode
  :pin melpa
  :mode (("\\.exs?\\'" . elixir-mode))
  :config (progn
            (add-hook 'elixir-mode-hook 'alex/set-elixir-format-hook)))

(defun alex/set-elixir-format-hook ()
  (add-hook 'before-save-hook 'elixir-format nil t))

(provide 'init-elixir)
