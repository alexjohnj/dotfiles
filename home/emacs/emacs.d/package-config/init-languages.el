;; init-languages -- Programming Language Configuration -*- lexical-binding:t -*-

(use-package treesit
  :straight nil
  :preface
  (defun alex/treesit-install-all-grammars ()
    "Install all language grammars in the variable 'treesit-language-source-alist'."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (let ((language (car grammar)))
        (unless (treesit-language-available-p language)
          (treesit-install-language-grammar language)))))

  (defun alex/treesit--add-source (lang &rest source)
    (eval-after-load 'treesit (lambda ()
                                (add-to-list 'treesit-language-source-alist `(,lang . ,source))))))

(require 'init-beancount-mode)

(use-package dockerfile-ts-mode
  :straight nil
  :mode "/Dockerfile"
  :init
  (alex/treesit--add-source 'dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"))

(use-package fish-mode
  :mode ("\\.fish\\'"))

(use-package git-modes
  :mode (("\\.gitignore\\'" . gitignore-mode)))

(use-package graphql-mode
  :mode (("\\.graphqls?\\'" . graphql-mode))
  :defer t)

(use-package json-ts-mode
  :straight nil
  :mode ("\\.json\\'" "\\.apns\\'")
  :init
  (alex/treesit--add-source 'json "https://github.com/tree-sitter/tree-sitter-json"))

(require 'init-latex)

(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode))
  :config
  (setopt markdown-italic-underscore t
          markdown-fontify-code-blocks-natively t
          markdown-enable-math t))

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode)
  :hook ((nix-ts-mode . eglot-ensure))
  :config
  (with-eval-after-load 'apheleia
    (let ((formatter (alist-get 'nix-mode apheleia-mode-alist)))
      (add-to-list 'apheleia-mode-alist (cons 'nix-ts-mode formatter)))))

(use-package css-ts-mode
  :straight nil
  :mode ("\\.css\\'")
  :init
  (alex/treesit--add-source 'css "https://github.com/tree-sitter/tree-sitter-css"))

(use-package js-ts-mode
  :straight nil
  :mode (("\\.js\\'" . js-ts-mode))
  :init
  (alex/treesit--add-source 'javascript "https://github.com/tree-sitter/tree-sitter-javascript"))

(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :init
  (alex/treesit--add-source 'typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  (alex/treesit--add-source 'tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
  :config

  ;; TODO: Make this a buffer local variable so that eslint is only used as a
  ;; formatter if it's available in the project?
  ;; Configure apheleia to run both prettier and eslint in that oredr to format JS buffers.
  ;; eslint is a pain because it needs a custom formatter to format files in place.
  ;; (with-eval-after-load 'apheleia
  ;;   (defconst alex/ESLINT-FORMATTER (expand-file-name "package-config/eslint-formatter.js" user-emacs-directory))
  ;;   (add-to-list 'apheleia-formatters
  ;;                '(eslint . (npx "eslint" "--fix-dry-run" "--stdin" "--stdin-filename" filepath
  ;;                                "--format" alex/ESLINT-FORMATTER)))
  ;;   (setf (alist-get 'typescript-ts-mode apheleia-mode-alist)
  ;;         '(prettier-typescript eslint))
  ;;   (setf (alist-get 'tsx-ts-mode apheleia-mode-alist)
  ;;         '(prettier-typescript eslint)))
  )

(use-package python-ts-mode
  :straight nil
  :mode ("\\.py\\'")
  :init
  (alex/treesit--add-source 'python "https://github.com/tree-sitter/tree-sitter-python"))

(use-package ruby-mode
  :mode "/\\(Gem\\|Fast\\|App\\|Match\\|Pod\\)file")

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . eglot-ensure)
  :config
  (setopt rust-mode-treesitter-derive t))

(use-package swift-mode
  :mode ("\\.swift\\'")
  :config
  (defun alex/swift-mode-hook ()
    (setq fill-column 120))
  (add-hook 'swift-mode-hook #'alex/swift-mode-hook))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-engine-detection t
        web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook #'aggressive-indent-mode))

(use-package yaml-mode
  :hook ((yaml-mode . highlight-indent-guides-mode)))

(provide 'init-languages)
