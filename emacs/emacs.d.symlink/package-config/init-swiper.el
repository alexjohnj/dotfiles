;;; init-swiper.el --- Swiper and Ivy configuration
;;; Commentary: https://github.com/abo-abo/swiper
(use-package swiper
  :ensure t
  :diminish (ivy-mode)
  :config (progn
            (global-set-key (kbd "C-s") 'swiper)
            (evil-leader/set-key "s" 'ivy-resume)
            (setq ivy-use-virtual-buffers t
                  projectile-completion-system 'ivy)
            (with-eval-after-load "ivy"
              (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
              (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
              (define-key ivy-minibuffer-map [escape] (kbd "C-g")))
            (ivy-mode)))

(use-package counsel
  :ensure t
  :config (progn
            (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
            (global-set-key (kbd "M-x") 'counsel-M-x)
            (global-set-key (kbd "C-x C-f") 'counsel-find-file)))

(provide 'init-swiper)
;;; init-swiper.el ends here
