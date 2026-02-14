;;; init-appearance.el --- Theme configuration -*- lexical-binding: t -*-

(use-package emacs
  :ensure nil
  :config
  (load-theme 'modus-vivendi t)
  ;; Match the fringe colour to the theme's background colour
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(use-package highlight-indent-guides
  :diminish
  :hook ((text-mode . highlight-indent-guides-mode)
         (prog-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))

(use-package rainbow-mode
  :diminish
  :hook ((prog-mode . rainbow-mode)))

;; Flash the mode line instead of playing the bell sound.
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Use my custom font if it's installed.
(defun alex/set-font ()
  (let ((font-name "Departure Mono"))
    (if (member font-name (font-family-list))
        (if alex/IS-MAC ; Font scaling is a bit different between macOS and other platforms.
            (set-face-attribute 'default nil :font font-name :height 130 :weight 'regular)
          (set-face-attribute 'default nil :font font-name :height 110))
      (warn "Font %s is not installed. Using the default font." font-name))))

;; Hook frame creation so the font is set when Emacs is running in server mode.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame (alex/set-font))))

;; Set the font immediately for when Emacs isn't running in server mode.
(alex/set-font)

;; Hide the titlebar when running on macOS
(when alex/IS-MAC
  (setq ns-use-proxy-icon nil
        frame-title-format nil))

(provide 'init-appearance)
