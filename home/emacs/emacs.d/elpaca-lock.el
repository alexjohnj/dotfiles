((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :ref "77115afc1b0b9f633084cf7479c767988106c196"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
                    (:package "aggressive-indent" :repo
                              "Malabarba/aggressive-indent-mode" :fetcher github
                              :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              treeless :ref
                              "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (annalist :source "elpaca-menu-lock-file" :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless
                     :ref "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia"
                     :files (:defaults ("scripts" "scripts/formatters")) :source
                     "MELPA" :protocol https :inherit t :depth treeless :ref
                     "143c1dffed15f1cab3eb06e148fe11224e39471c"))
 (auctex :source "elpaca-menu-lock-file" :recipe
         (:package "auctex" :repo
                   ("https://git.savannah.gnu.org/git/auctex.git" . "auctex")
                   :branch "main" :files ("*" (:exclude ".git")) :source
                   "GNU ELPA" :protocol https :inherit t :depth treeless :ref
                   "79e0b72b347f801526e98872c4b940d866d78e1d"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "2b2a5c5bef16eddcce507d9b5804e5a0cc9481ae"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "d1d39d52151a10f7ca29aa291886e99534cc94db"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/*")
                  :fetcher github :source "MELPA" :protocol https :inherit t
                  :depth treeless :ref
                  "abfe0003d71b61ffdcf23fc6e546643486daeb69"))
 (ctrlf :source "elpaca-menu-lock-file" :recipe
        (:package "ctrlf" :fetcher github :repo "radian-software/ctrlf" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless
                  :ref "3e1d4d74b201e45a2d471675f5fdae370f23f947"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :protocol https
                 :inherit t :depth treeless :ref
                 "fb443e7a6e660ba849cafcd01021d9aac3ac6764"))
 (deft :source "elpaca-menu-lock-file" :recipe
       (:package "deft" :repo "jrblevin/deft" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "b369d7225d86551882568788a23c5497b232509c"))
 (diminish :source "elpaca-menu-lock-file" :recipe
           (:package "diminish" :fetcher github :repo "myrjola/diminish.el"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless
                     :ref "fbd5d846611bad828e336b25d2e131d1bc06b83d"))
 (dumb-jump :source "elpaca-menu-lock-file" :recipe
            (:package "dumb-jump" :repo "jacktasia/dumb-jump" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "e19593b720b9db5cf8d61d28d466da30b449b84b"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo ("https://codeberg.org/akib/emacs-eat" . "eat")
                :files ("*" (:exclude ".git")) :source "NonGNU ELPA" :protocol
                https :inherit t :depth treeless :ref
                "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (eldoc-box :source "elpaca-menu-lock-file" :recipe
            (:package "eldoc-box" :repo "casouri/eldoc-box" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "595262ec8ff56e8f86ef77d8e69339e84117e5f0"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "1508298c1ed19c81fa4ebc5d22d945322e9e4c52" :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info) :source
                               "Elpaca extensions" :protocol https :inherit t
                               :depth treeless :ref
                               "1508298c1ed19c81fa4ebc5d22d945322e9e4c52"))
 (envrc :source "elpaca-menu-lock-file" :recipe
        (:package "envrc" :fetcher github :repo "purcell/envrc" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless
                  :ref "d3289e92e0e2fc39a2be3f3bb382f99a9c5dc8e3"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi"
                            (:exclude "evil-test-helpers.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless
                 :wait t :ref "729d9a58b387704011a115c9200614e32da3cefc"))
 (evil-cleverparens :source "elpaca-menu-lock-file" :recipe
                    (:package "evil-cleverparens" :fetcher github :repo
                              "emacs-evil/evil-cleverparens" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth
                              treeless :ref
                              "4c413a132934695b975004d429b0b0a6e3d8ca38"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-collection" :fetcher github :repo
                            "emacs-evil/evil-collection" :files
                            (:defaults "modes") :source "MELPA" :protocol https
                            :inherit t :depth treeless :wait t :ref
                            "d052ad2ec1f6a4b101f873f01517b295cd7dc4a9"))
 (evil-commentary :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-commentary" :repo "linktohack/evil-commentary"
                            :fetcher github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth
                            treeless :ref
                            "c5945f28ce47644c828aac1f5f6ec335478d17fb"))
 (evil-escape :source "elpaca-menu-lock-file" :recipe
              (:package "evil-escape" :fetcher github :repo
                        "emacsorphanage/evil-escape" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth
                        treeless :ref "aebd1a78a6bd33e5164e7552096b3fe1172d3012"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround"
                          :fetcher github :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth
                          treeless :ref
                          "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                  "*.texinfo" "doc/dir" "doc/*.info"
                                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info" "docs/*.texi"
                                  "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el" "*-tests.el"
                                            "LICENSE" "README*" "*-pkg.el"))
                                 :source "MELPA" :protocol https :inherit t
                                 :depth treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"))
 (fish-mode :source "elpaca-menu-lock-file" :recipe
            (:package "fish-mode" :fetcher github :repo "wwwjfy/emacs-fish"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (general :source "elpaca-menu-lock-file" :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes"
                      :old-names
                      (gitattributes-mode gitconfig-mode gitignore-mode) :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "c3faeeea1982786f78d8c38397dec0f078eaec84"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo
                            "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth
                            treeless :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless
                     :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (graphql-mode :source "elpaca-menu-lock-file" :recipe
               (:package "graphql-mode" :repo "davazp/graphql-mode" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth
                         treeless :ref
                         "ef757c6ce226ebabc834d49db5161ec90cf82202"))
 (highlight-indent-guides :source "elpaca-menu-lock-file" :recipe
                          (:package "highlight-indent-guides" :fetcher github
                                    :repo "DarthFennec/highlight-indent-guides"
                                    :files
                                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                     "*.texinfo" "doc/dir" "doc/*.info"
                                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                     "docs/dir" "docs/*.info" "docs/*.texi"
                                     "docs/*.texinfo"
                                     (:exclude ".dir-locals.el" "test.el"
                                               "tests.el" "*-test.el"
                                               "*-tests.el" "LICENSE" "README*"
                                               "*-pkg.el"))
                                    :source "MELPA" :protocol https :inherit t
                                    :depth treeless :ref
                                    "802fb2eaf67ead730d7e3483b9a1e9639705f267"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv"
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (kdl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "kdl-mode" :fetcher github :repo
                     "taquangtrung/emacs-kdl-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless
                     :ref "2d849e298199f490e4894c01764a8a83decd704a"))
 (kind-icon :source "elpaca-menu-lock-file" :recipe
            (:package "kind-icon" :repo
                      ("https://github.com/jdtsmith/kind-icon" . "kind-icon")
                      :files ("*" (:exclude ".git")) :source "GNU ELPA"
                      :protocol https :inherit t :depth treeless :ref
                      "556b0fb92aac24979b2c501431c7d48f75a5169f"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "MELPA" :protocol https
                  :inherit t :depth treeless :ref
                  "2a89ba755b0459914a44b1ffa793e57f759a5b85"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless
                  :ref "96d274457baea419fe7b3acbc955c8527d720024"))
 (magit-delta :source "elpaca-menu-lock-file" :recipe
              (:package "magit-delta" :fetcher github :repo
                        "dandavison/magit-delta" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth
                        treeless :ref "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit"
                          :files
                          ("lisp/magit-section.el" "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "MELPA" :protocol https :inherit t :depth
                          treeless :ref
                          "96d274457baea419fe7b3acbc955c8527d720024"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :ref "0d08fbea0f1182627891240780081ba528c1348b"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth
                          treeless :ref
                          "9de2df5a9f2f864c82ec112d3369154767a2bb49"))
 (nix-ts-mode :source "elpaca-menu-lock-file" :recipe
              (:package "nix-ts-mode" :fetcher github :repo
                        "nix-community/nix-ts-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth
                        treeless :ref "625306cf9cd99390aa032563f44157ee25ad66d5"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (page-break-lines :source "elpaca-menu-lock-file" :recipe
                   (:package "page-break-lines" :fetcher github :repo
                             "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth
                             treeless :ref
                             "f54aa2b96f6ed249e103346cdb872c97c3c98054"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
                    "https://paredit.org/paredit.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "af075775af91f2dbc63b915d762b4aec092946c4"))
 (popup :source "elpaca-menu-lock-file" :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless
                  :ref "45a0b759076ce4139aba36dde0a2904136282e73"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth
                               treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
               (:package "rainbow-mode" :repo
                         ("https://github.com/emacsmirror/gnu_elpa"
                          . "rainbow-mode")
                         :branch "externals/rainbow-mode" :files
                         ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
                         https :inherit t :depth treeless :ref
                         "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (rg :source "elpaca-menu-lock-file" :recipe
     (:package "rg" :fetcher github :repo "dajva/rg.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "a614e7d7709c7bf5c5accff4003d351c3f28ee98"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "f68ddca5c22b94a2de7c9ce20d629cd78d60b269"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo "Fuco1/smartparens"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth
                        treeless :ref "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (svg-lib :source "elpaca-menu-lock-file" :recipe
          (:package "svg-lib" :repo
                    ("https://github.com/rougier/svg-lib" . "svg-lib") :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https
                    :inherit t :depth treeless :ref
                    "925ed4a0215c197ba836e7810a93905b34bea777"))
 (swift-mode :source "elpaca-menu-lock-file" :recipe
             (:package "swift-mode" :repo "swift-emacs/swift-mode" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :ref "cfae3b85ad09bd293df941261afbc21e41bbb5f8"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :wait t :ref "bda7c2e0772deaee8e36a217d15c14784e8c6800"))
 (undo-fu :source "elpaca-menu-lock-file" :recipe
          (:package "undo-fu" :fetcher codeberg :repo "ideasman42/emacs-undo-fu"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "b4ce5ed20c1cf0591e497e6998a7634a172726fa"))
 (unfill :source "elpaca-menu-lock-file" :recipe
         (:package "unfill" :fetcher github :repo "purcell/unfill" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless
                   :ref "4a15511876983eeaa75e57fcab8d4d51fe9b3840"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher github
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "93f15873d7d6244d72202c5dd7724a030a2d5b9a"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless
                     :ref "1e7694aee87722f9e51b6e39c35d175d83a1fb2c"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep"
                  :files ("wgrep.el") :source "MELPA" :protocol https :inherit t
                  :depth treeless :ref
                  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "902b4d572af2c2f36060da01e3c33d194cdec32b"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git"
                      :branch "elpa/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher
                        github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth
                        treeless :ref "86fab1d247eb5ebe6b40fa5073a70dfa487cd465"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "d91f878729312a6beed77e6637c60497c5786efa")))
