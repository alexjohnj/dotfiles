(use-package powerline
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (progn
        (load-library "powerline-srgb-offset")
        (powerline-srgb-offset-add-theme "base16-eighties"
                                         '('("mode-line" "#646464")
                                           '("powerline-active1" "#484848")
                                           '("powerline-active2" "#797979")
                                           '("mode-line-inactive" "#494949")
                                           '("powerline-inactive1" "#242424")
                                           '("powerline-inactive2" "#424242")))
        (powerline-srgb-offset-activate "base16-eighties")))
  (powerline-default-theme))

(provide 'init-powerline)
