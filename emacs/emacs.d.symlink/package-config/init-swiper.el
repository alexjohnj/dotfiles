;;; init-swiper.el --- Swiper and Ivy configuration
;;; Commentary: https://github.com/abo-abo/swiper
(use-package swiper
  :ensure t
  :diminish (ivy-mode)
  :init (progn
          (setq ivy-use-virtual-buffers t
                projectile-completion-system 'ivy)
          (global-set-key (kbd "C-s") 'swiper)
          (with-eval-after-load "ivy"
            (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
            (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
            (define-key ivy-minibuffer-map [escape] (kbd "C-g"))))
  :config (progn
            (ivy-mode)))

(provide 'init-swiper)
;;; init-swiper.el ends here
