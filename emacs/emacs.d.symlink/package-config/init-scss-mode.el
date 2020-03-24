;;; init-scss-mode --- Configuration for scss-mode
;;; Commentary:
;;  Uses the package from https://github.com/antonj/scss-mode
;;; Code:
(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :init (progn
          (setq scss-compile-at-save nil)))

(provide 'init-scss-mode)
;;; init-scss-mode.el ends here
