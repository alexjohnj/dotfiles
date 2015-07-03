(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)

;; Bootstrap `use-package'
; From: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package evil
	     :ensure t
	     :config (evil-mode 1))
