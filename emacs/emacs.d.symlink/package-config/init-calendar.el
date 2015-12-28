;;; init-calendar.el --- Configuration for Emacs' calendar-mode
;;; Commentary: This is mostly just evil key bindings
(use-package calendar
  :commands (calendar)
  :init (progn
          (evil-define-key 'motion calendar-mode-map
            "j" 'calendar-forward-week
            "k" 'calendar-backward-week
            "h" 'calendar-backward-day
            "l" 'calendar-forward-day
            "}" 'calendar-forward-month
            "{" 'calendar-backward-month
            (kbd "C-f") 'calendar-scroll-left-three-months
            (kbd "C-b") 'calendar-scroll-right-three-months
            ">" 'calendar-scroll-left
            "<" 'calendar-scroll-right
            "$" 'calendar-end-of-week
            "^" 'calendar-beginning-of-week)
          (evil-leader/set-key-for-mode 'calendar-mode
            "m @" 'calendar-set-mark
            "m !" 'calendar-unmark
            "m g d" 'calendar-goto-date
            "m g D" 'calendar-goto-day-of-year
            "m g w" 'calendar-iso-goto-week

            "m p d" 'calendar-print-day-of-year
            "m p j" 'calendar-julian-print-date)))
(provide 'init-calendar)
;;; init-calendar.el ends here
