;;;_. Initialization

;;;_ , Get MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;;;_ , Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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
(bind-key "C-x k" 'kill-this-buffer)

;;;_  . C-c ?

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;;;_. Packages

;;;_ , allout
(use-package allout
  :diminish allout-mode
  :commands allout-mode)

;;;_ , cryptol-mode
(use-package cryptol-mode :ensure t)

;;;_ , exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;;;_ , haskell-mode
(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :config
  (progn
    (defun my-haskell-interactive-mode-hook ()
      ;; disable trailing whitespace in interactive mode
      (setq show-trailing-whitespace nil))
    (add-hook 'haskell-interactive-mode-hook 'my-haskell-interactive-mode-hook)
    (when (executable-find "hasktags")
      (setq haskell-tags-on-save t))))

;;;_ , helm
(use-package helm
  :ensure t
  :commands helm-mode
  :init
  (progn
    (use-package helm-config)

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

    (use-package helm-adaptive
      :init
      (helm-adaptive-mode t))

    ;; For some reason, the :diminish feature of use-package doesn't
    ;; do the trick with helm-mode, so we just diminish it explicitly
    ;; after initializing
    (defun my-helm-init-hook ()
      (helm-mode)
      (diminish 'helm-mode))

    (add-hook 'after-init-hook 'my-helm-init-hook)

    ;; Other helm packages
    (use-package helm-ag :ensure t)
    (use-package helm-idris :ensure t)
    (use-package helm-projectile :ensure t)
    (use-package helm-swoop :ensure t)
    )

  :config
  (progn
    ;; Swap the tab and C-z bindings in helm buffers, in particular so
    ;; that completion is more like a tab-completing shell
    (bind-key "<tab>"   'helm-execute-persistent-action helm-map)
    (bind-key "C-i"     'helm-execute-persistent-action helm-map)
    (bind-key "C-z"     'helm-select-action helm-map)))

;;;_ , idris-mode
(use-package idris-mode
  :ensure t)

;;;_ , magit
(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-c g" . magit-status)))

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
    (add-hook 'org-capture-mode-hook 'auto-fill-mode)))

;;;_ , projectile
(use-package projectile
  :ensure t
  :commands projectile-global-mode
  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode 1))
  :config
  (progn
    (bind-key "s s" 'helm-projectile-ag projectile-command-map)))

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

;;;_ , solarized-theme
(use-package color-theme-sanityinc-solarized
  :ensure t
  :commands (color-theme-sanityinc-solarized-dark color-theme-sanityinc-solarized-light)
  :if (display-graphic-p))

;;;_ , structured-haskell-mode
(use-package shm
  :load-path "site-lisp/structured-haskell-mode/elisp"
  :init
  (progn
    (setq shm-program-name
          (expand-file-name
            (concat (file-name-as-directory user-emacs-directory)
                    "site-lisp/structured-haskell-mode/.cabal-sandbox/bin/structured-haskell-mode")))
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)))

;;;_ , whitespace
(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook 'whitespace-mode)
    ))

;;;_ , windmove
(use-package windmove
  :init (windmove-default-keybindings))

;;;_. Post-initialization

;;;_. Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(display-time-mode t)
 '(explicit-shell-file-name "/usr/local/bin/zsh")
 '(global-whitespace-mode t)
 '(haskell-completing-read-function (quote helm--completing-read-default))
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-decl-scan turn-on-haskell-doc interactive-haskell-mode)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(helm-M-x-fuzzy-match t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-command-prefix-key "C-x h")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("u" "Agenda and all Unscheduled"
      ((agenda "" nil)
       (tags-todo "+CATEGORY=\"Unscheduled\"" nil))
      nil))))
 '(org-agenda-files (quote ("~/AeroFS/org/tasks.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-capture-templates
   (quote
    (("n" "Add new note" entry
      (file "~/AeroFS/org/notes.org")
      "* %?
  Added: %u")
     ("t" "Add new task" entry
      (file+headline "~/AeroFS/org/tasks.org" "Unscheduled")
      "* TODO %?
  Added: %u"))))
 '(org-default-notes-file "~/AeroFS/org/notes.org")
 '(org-directory "~/AeroFS/org")
 '(org-refile-use-cache t)
 '(org-reverse-note-order t)
 '(projectile-completion-system (quote helm))
 '(projectile-enable-caching t)
 '(scroll-bar-mode nil)
 '(show-trailing-whitespace t)
 '(term-buffer-maximum-size 10000)
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face tabs trailing lines space-before-tab newline empty space-after-tab tab-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;_. Terminal-specific

(when (not (display-graphic-p))
  ;; The solarized theme ends up in the customize variables; disable
  ;; it here in case we're in that situation.
  (disable-theme 'sanityinc-solarized-dark)
  )

;;;_. Graphical-specific

(when (display-graphic-p)
  ;; Terminal emacs just inherits from a Solarized terminal emulator
  (color-theme-sanityinc-solarized-dark)

  ;; Embiggening again is handled by the terminal when non-graphical
  (cond
   ((find-font (font-spec :name "Menlo"))
    (set-frame-font "Menlo 14"))
   ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-frame-font "DejaVu Sans Mono 14")))

  ;; No accidental minimizing
  (unbind-key "C-x C-z")

  ;; Fullscreen
  (toggle-frame-fullscreen)
  )

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
