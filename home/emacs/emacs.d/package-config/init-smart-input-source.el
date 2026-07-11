;;; init-smart-input-source.el --- Automatic input source switching -*- lexical-binding: t -*-

;; `respect-mode' switches to English when leaving evil insert state.
;; `context-mode' looks at the text around point when entering evil insert
;; state and switches back to whichever source matches it -- this is what
;; makes the source come back on re-entering insert, not just leave it.
(use-package sis
  :hook ((alex-first-input . sis-global-respect-mode)
         (alex-first-input . sis-global-context-mode))
  :config
  (cond
   (alex/IS-MAC
    (sis-ism-lazyman-config "com.apple.keylayout.Colemak"
                            "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese"
                            'macism))
   ((eq system-type 'gnu/linux)
    ;; fcitx5 tracks its own input sources, so these are ignored.
    (sis-ism-lazyman-config nil nil 'fcitx5))))

(provide 'init-smart-input-source)
