;;;_. Initialization

;;;_ , Get MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;;;_ , Bootstrap use-package
(eval-when-compile
  (progn
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)))
(use-package diminish :ensure t)
(require 'bind-key)

;;;_. Platform-specific

;;;_ , OS X
(when (eq system-type 'darwin)
  (let ((ls-command (or (executable-find "gls")
                        (executable-find "/opt/local/bin/gls")
                        (executable-find "/usr/local/bin/gls"))))
    (if (and ls-command (file-exists-p ls-command))
        (setq insert-directory-program ls-command)
      (message "Note: this Mac doesn't have an ls that supports --dired"))))

;;;_. Keybindings

;;;_ , global-map

;;;_  . C-?

(bind-key* "<C-return>" 'other-window)

;;;_  . C-x ?

;; I want to unlearn C-x o for switching windows
(unbind-key "C-x o")

;; I never want to kill a buffer that's not the current one
(bind-key "C-x k" 'kill-current-buffer)

;; Unbind for helm prefix later
(unbind-key "C-x h")

;;;_  . C-c ?

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;;;_. Packages

;;;_ , allout
(use-package allout
  :diminish allout-mode
  :commands allout-mode)

;;;_ , AUCTeX
(use-package tex-site
  :ensure auctex
  :config (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;;;_ , auctex-latexmk
(use-package auctex-latexmk
  :ensure t
  :config (auctex-latexmk-setup))

;;;_ , c-mode
(defun my-c-initialization-hook ()
  (bind-key "C-c C-f" 'clang-format-buffer c-mode-map))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-hook ()
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;;;_ , cmake-mode
(use-package cmake-mode :ensure t)

;;;_ , company
(use-package company
  :ensure t
  :demand
  :diminish company-mode
  :commands company-mode)

;;;_ , cryptol-mode
(use-package cryptol-mode :ensure t)

;;;_ , edit-indirect
(use-package edit-indirect :ensure t)

;;;_ , editorconfig-mode
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;;_ , elm-mode
(use-package elm-mode :ensure t)

;;;_ , emojify-mode
(use-package emojify
  :ensure t
  :config
  (progn
    (global-emojify-mode)
    (global-emojify-mode-line-mode)))

;;;_ , exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;;;_ , flycheck-haskell
(use-package flycheck-haskell :ensure t)

;;;_ , groovy-mode
(use-package groovy-mode :ensure t :commands groovy-mode)

;;;_ , haskell-mode
(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
    (setq haskell-process-args-cabal-repl
          '("--ghc-options=-fobject-code" "--ghc-options=-ferror-spans"))
    (setq haskell-process-auto-import-loaded-modules t)
    (setq haskell-process-log t)
    (setq haskell-process-reload-with-fbytecode t)
    (setq haskell-process-suggest-add-package nil)

    ;; automatically choose cabal repl, stack ghci, etc
    (setq haskell-process-type 'cabal-new-repl)

    ;; only run hasktags if it's available on the system
    (when (executable-find "hasktags")
      (setq haskell-tags-on-save t))))

;;;_ , helm
(use-package helm
  :ensure t
  :commands helm-mode
  :init
  (progn
    ;; Configuration
    (setq helm-command-prefix-key "C-x h")
    (setq helm-M-x-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t)

    ;; Override basic emacs commands
    (bind-key "M-x"     'helm-M-x)
    (bind-key "C-x C-f" 'helm-find-files)
    (bind-key "C-x C-b" 'helm-buffers-list)
    (bind-key "C-x b"   'helm-mini)
    (bind-key "M-y"     'helm-show-kill-ring)

    (use-package helm-descbinds
      :ensure t
      :commands helm-descbinds
      :init
      (fset 'describe-bindings 'helm-descbinds))

    (bind-key "C-h b" 'helm-descbinds)

    (use-package helm-adaptive)

    ;; For some reason, the :diminish feature of use-package doesn't
    ;; do the trick with helm-mode, so we just diminish it explicitly
    ;; after initializing
    (defun my-helm-init-hook ()
      (helm-mode)
      (diminish 'helm-mode)
      (helm-adaptive-mode t))

    (add-hook 'after-init-hook 'my-helm-init-hook)

    ;; Other helm packages
    (use-package helm-idris :ensure t)
    (use-package helm-projectile :ensure t)
    (use-package helm-swoop :ensure t))

  :config
  (progn
    ;; Swap the tab and C-z bindings in helm buffers, in particular so
    ;; that completion is more like a tab-completing shell
    (bind-key "<tab>"   'helm-execute-persistent-action helm-map)
    (bind-key "C-i"     'helm-execute-persistent-action helm-map)
    (bind-key "C-z"     'helm-select-action helm-map)
    (bind-key "C-l"     'helm-grep-run-ag-grep-parent-directory helm-map)))


;;;_ , idris-mode
(use-package idris-mode
  :ensure t)

;;;_ , lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((rust-mode) . lsp)
  :bind (:map lsp-command-map
              ("C-S-l" . lsp-keymap-prefix)))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

;;;_ , magit
(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-c g" . magit-status))
  :config
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")))
;; Too slow for now on big diffs
;; (use-package magit-delta
;;   :ensure t
;;   :hook (magit-mode . magit-delta-mode))
(use-package magit-todos :ensure t)

;;;_ , markdown-mode
(defun my-markdown-initialization-hook ()
  (setq-local fill-column 80)
  (setq markdown-fontify-code-blocks-natively t))
(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :hook (markdown-mode . my-markdown-initialization-hook))

;;;_ , meson-mode
(use-package meson-mode
  :ensure t
  :commands meson-mode)

;;;_ , nix-mode
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;;_ , obsidian
(use-package obsidian
  :ensure t
  :config
  (global-obsidian-mode t)
  (obsidian-backlinks-mode t)
  ;; customize obsidian directories in `site-lisp` local config
  )

;;;_ , org
(use-package org
  :ensure t
  :commands org-mode
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :init
  (progn
    (add-hook 'org-capture-mode-hook 'auto-fill-mode)
    (setq org-agenda-custom-commands
          '(("n" "Agenda and all TODOs"
             ((agenda "" nil)
              (alltodo "" nil))
             nil)
            ("u" "Agenda and all Unscheduled"
             ((agenda "" nil)
              (tags-todo "+CATEGORY=\"Unscheduled\"" nil))
             nil)))
    (setq org-agenda-files (quote ("~/acfoltzer@fastly.com/org/tasks.org.txt")))
    (setq org-agenda-ndays 7)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-capture-templates
          (quote
           (("n" "Add new note" entry
             (file "~/acfoltzer@fastly.com/org/notes.org.txt")
             "* %?
  Added: %u")
            ("t" "Add new task" entry
             (file+headline "~/acfoltzer@fastly.com/org/tasks.org.txt" "Unscheduled")
             "* TODO %?
  Added: %u"))))
    (setq org-default-notes-file "~/acfoltzer@fastly.com/org/notes.org.txt")
    (setq org-directory "~/acfoltzer@fastly.com/org")
    (setq org-refile-use-cache t)
    (setq org-reverse-note-order t)))

;;;_ , prog-mode
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;_ , projectile
(use-package projectile
  :ensure t
  :commands projectile-mode
  :diminish projectile-mode
  :init
  (progn
    (projectile-mode 1)
    (setq projectile-completion-system 'helm)
    (setq projectile-enable-caching t))

  :config
  ;; Invalidate projectile cache when checking out a new branch
  (defun run-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))
  (advice-add 'magit-checkout
              :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-branch-and-checkout ; This is `b c'.
              :after #'run-projectile-invalidate-cache)

  (progn
    (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
    (bind-key "s s" 'helm-projectile-ag projectile-command-map)))

;;;_ , purescript
(use-package purescript-mode
  :ensure t
  :init
  (progn
    (add-hook 'purescript-mode-hook '(turn-on-purescript-indentation))))

;;;_ , python
(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))

    (setq indicate-empty-lines t)
    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil)

    (bind-key "C-c C-z" #'python-shell python-mode-map)
    (unbind-key "C-c c" python-mode-map))

  (add-hook 'python-mode-hook 'my-python-mode-hook))

;;;_ , recentf
(use-package recentf
  :if (not noninteractive)
  :init
  (progn
    (recentf-mode 1)

    (defun recentf-add-dired-directory ()
      (if (and dired-directory
               (file-directory-p dired-directory)
               (not (string= "/" dired-directory)))
          (let ((last-idx (1- (length dired-directory))))
            (recentf-add-file
             (if (= ?/ (aref dired-directory last-idx))
                 (substring dired-directory 0 last-idx)
               dired-directory)))))

    (add-hook 'dired-mode-hook 'recentf-add-dired-directory)))

;;;_ , rust
(use-package rust-mode
  :ensure t
  :commands rust-mode)

(use-package racer
  :ensure t
  :diminish
  :bind (:map company-mode-map ("TAB" . company-indent-or-complete-common))
  :init
  (progn
    ;; (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (setq company-tooltip-align-annotations t)))

;;;_ , scheme
(use-package scheme
  :init
  (progn
    (setq scheme-program-name "petite")))

;; ;;;_ , smart-tabs
;; (use-package smart-tabs-mode
;;   :ensure t
;;   :init
;;   (progn
;;     (smart-tabs-insinuate 'c)))

;;;_ , solarized-theme
(use-package color-theme-sanityinc-solarized
  :ensure t
  :commands (color-theme-sanityinc-solarized-dark color-theme-sanityinc-solarized-light)
  :if (display-graphic-p))

;;;_ , structured-haskell-mode
;; (use-package shm
;;   :load-path "site-lisp/structured-haskell-mode/elisp"
;;   :init
;;   (progn
;;     (setq shm-program-name
;;           (expand-file-name
;;             (concat (file-name-as-directory user-emacs-directory)
;;                     "site-lisp/structured-haskell-mode/.cabal-sandbox/bin/structured-haskell-mode")))
;;     (when (executable-find shm-program-name)
;;       (add-hook 'haskell-mode-hook 'structured-haskell-mode))))

(use-package tramp :ensure t)

;;;_ , unfill
(use-package unfill :ensure t)

;;;_ , unicode-fonts
(use-package unicode-fonts
  :ensure t
  :init (unicode-fonts-setup))

;;;_ , vcl-mode
(use-package vcl-mode :ensure t)

;;;_ , virtualenvwrapper
(use-package virtualenvwrapper :ensure t)

;;;_ , wgrep
(use-package wgrep :ensure t)

;;;_ , whitespace
(use-package whitespace
  :ensure t
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :init
  (progn
    ;; (add-hook 'prog-mode-hook 'whitespace-mode)
    (global-whitespace-mode -1)
    (setq whitespace-style
          '(face tabs trailing lines space-before-tab
            newline empty space-after-tab tab-mark))))

;;;_ , windmove
(use-package windmove
  :init (windmove-default-keybindings))

;;;_ , yaml
(use-package yaml-mode :ensure t)

;;;_ , yasnippet
(use-package yasnippet
  :ensure t
  :hook ((rust-mode) . yas-minor-mode))

;;;_. Post-initialization

;;;_. Customize

;;;_ , Mode line
(column-number-mode 1)
(display-time-mode 1)
;; Hide the `Git:branch` line that's often inaccurate with magit
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; No startup screen
(setq inhibit-startup-screen t)

;; Don't prompt to reload TAGS file when it changes, and always reload
;; when we change directories. This makes haskell-mode far less
;; annoying
(setq tags-revert-without-query t)
(setq tags-add-tables nil)

;; No indenting with tabs by default
(setq-default indent-tabs-mode nil)

;; Never indent with tabs for align-regexp, even for tab-sensitive
;; modes like Makefiles
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Configure some C-mode indenting that will percolate to inherited
;; modes
(c-set-offset 'inextern-lang 0 t)
(c-set-offset 'innamespace 0 t)
(c-set-offset 'namespace-open  0 t)
(c-set-offset 'namespace-close 0 t)

;; Don't split horizontally when making new windows
(setq split-width-threshold 9999)

;; Automatically save desktop
(desktop-save-mode 1)

;; Wrap to column 100
(setq-default fill-column 100)

;; no more yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; stop asking about loading files unless they're 1GB or bigger
(setq large-file-warning-threshold (* 1024 1024 1024))

;; Treat .mjs files as JavaScript
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

;; Increase the limit of data for Emacs to read from subprocesses. Useful for large LSP responses.
(setq read-process-output-max (* 1024 1024))

;;;_. Local configuration

;; Anything specific to a machine should be in site-lisp/local-config
;; so it's not checked in, but don't fail if it's not present.
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'local-config nil t)

;;;_. Terminal-specific

(when (not (display-graphic-p))
  ;; The solarized theme ends up in the customize variables; disable
  ;; it here in case we're in that situation.
  (disable-theme 'sanityinc-solarized-dark)
  )

;;;_. Graphical-specific

(when (display-graphic-p)
  ;; No useless toolbar or menu bar
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  ;; No scrollbar
  (scroll-bar-mode -1)

  ;; Embiggening again is handled by the terminal when non-graphical
  (cond
   ((find-font (font-spec :name "JetBrains Mono"))
    (set-frame-font "JetBrains Mono 18"))
   ((find-font (font-spec :name "Menlo"))
    (set-frame-font "Menlo 18")))

  ;; No accidental minimizing
  (unbind-key "C-x C-z")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(auth-source-save-behavior nil)
 '(beacon-color "#d33682")
 '(cryptol-command "/opt/cryptol/bin/cryptol")
 '(custom-enabled-themes '(sanityinc-solarized-dark))
 '(custom-safe-themes
   '("48d34b6afe72407ca494387c8bea495bb2deee96bd88516f302db1f11e1810a1"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(emojify-emoji-set "twemoji-v2-22")
 '(fci-rule-color "#073642")
 '(flycheck-checker-error-threshold 1024)
 '(frame-background-mode 'dark)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(global-emojify-mode t)
 '(helm-always-two-windows nil)
 '(helm-grep-file-path-style 'relative)
 '(helm-projectile-set-input-automatically nil)
 '(helm-split-window-default-side 'same)
 '(helm-window-prefer-horizontal-split t)
 '(lsp-file-watch-threshold 10000)
 '(lsp-go-gopls-server-path "~/go/bin/gopls")
 '(lsp-keymap-prefix "C-S-l")
 '(lsp-prefer-flymake nil)
 '(lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
 '(lsp-rust-analyzer-cargo-run-build-scripts t)
 '(lsp-rust-analyzer-import-enforce-granularity t)
 '(lsp-rust-analyzer-import-granularity "module")
 '(lsp-rust-analyzer-import-merge-behaviour "last")
 '(lsp-rust-analyzer-proc-macro-enable t)
 '(lsp-rust-cfg-test t)
 '(lsp-rust-server 'rust-analyzer)
 '(lsp-rust-unstable-features t)
 '(magit-commit-arguments '("--gpg-sign=2A91B421C62B535C"))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(mermaid-mmdc-location "/home/acfoltzer/bin/mmdc/node_modules/.bin/mmdc")
 '(package-selected-packages
   '(auctex-latexmk cmake-mode color-theme-sanityinc-solarized company cryptol-mode diminish
                    edit-indirect elm-mode emojify exec-path-from-shell flycheck-haskell groovy-mode
                    helm-ag helm-descbinds helm-idris helm-projectile helm-swoop lsp-ui magit-todos
                    meson-mode nix-mode obsidian purescript-mode racer unfill unicode-fonts vcl-mode
                    virtualenvwrapper yaml-mode yasnippet))
 '(require-final-newline t)
 '(rust-format-goto-problem nil)
 '(safe-local-variable-values
   '((lsp-rust-all-features . t) (eval c-set-offset 'innamespace 0)
     (eval when (fboundp 'c-toggle-comment-style) (c-toggle-comment-style 1))))
 '(saw-script-command "/opt/saw/bin/saw")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198")
     (120 . "#268bd2") (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16")
     (220 . "#b58900") (240 . "#859900") (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682")
     (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Run rustfmt on the active region
(defun rustfmt-region (&optional b e) 
  (interactive "r")
  (call-process-region b e "rustfmt" t t))

(defun int-to-hex (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "python3 -c \"import sys;[sys.stdout.write(hex(int(line))) for line in sys.stdin]\"" t t))

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
