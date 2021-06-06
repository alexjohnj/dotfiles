(use-package ediff
  :commands (ediff)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  (defun ediff-copy-both-to-C ()
    "Merge the diff in buffer A and buffer B into C"
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (general-def ediff-mode-map
    "D" #'ediff-copy-both-to-C))

(provide 'init-ediff)
