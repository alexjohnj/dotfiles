(use-package elixir-mode
  :ensure t
  :mode "\\.ex\\'"
  :mode "\\.exs\\'"
  :pin melpa
  :config (progn
            (add-hook 'elixir-mode-hook (lambda ()
                                          (add-hook 'before-save-hook 'elixir-format nil t)))
            (add-hook 'elixir-mode-hook 'aggressive-indent-mode)))

(provide 'init-elixir)
