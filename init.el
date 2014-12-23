;;;_. Initialization

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

;;;_. Keybindings

;;;_ , global-map

;;;_  . C-?

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(bind-key* "<C-return>" 'other-window)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
      (bury-buffer)))

(bind-key "C-z" 'collapse-or-expand)

;;;_  . C-c ?

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;;;_. Packages

;;;_ , allout
(use-package allout
  :diminish allout-mode
  :commands allout-mode)

;;;_ , exec-path-from-shell
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;;;_ , haskell-mode
(use-package haskell-mode
  :commands haskell-mode
  :config
  (progn
    (when (executable-find "hasktags")
      (setq haskell-tags-on-save t))))

;;;_ , helm
(use-package helm
  :commands helm-mode
  :init
  (progn
    (use-package helm-config)

    ;; Override basic emacs commands
    (bind-key "M-x"	'helm-M-x)
    (bind-key "C-x C-f" 'helm-find-files)
    (bind-key "C-x C-b" 'helm-buffers-list)
    (bind-key "C-x b"   'helm-mini)
    (bind-key "M-y"     'helm-show-kill-ring)

    (use-package helm-descbinds
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
      (progn
	(helm-mode)
	(diminish 'helm-mode)))
    
    (add-hook 'after-init-hook 'my-helm-init-hook)
    )

  :config
  (progn
    ;; Swap the tab and C-z bindings in helm buffers, in particular so
    ;; that completion is more like a tab-completing shell
    (bind-key "<tab>"   'helm-execute-persistent-action helm-map)
    (bind-key "C-i"     'helm-execute-persistent-action helm-map)
    (bind-key "C-z"     'helm-select-action helm-map)))

;;;_ , org
(use-package org
  :commands org-mode
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link)))

;;;_ , projectile
(use-package projectile
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
  :commands (color-theme-sanityinc-solarized-dark color-theme-sanityinc-solarized-light)
  :if (display-graphic-p))


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
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(haskell-completing-read-function (quote helm--completing-read-default))
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-doc turn-on-haskell-indentation interactive-haskell-mode)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(helm-M-x-fuzzy-match t)
 '(helm-buffers-fuzzy-matching t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/AeroFS/org/tasks.org")))
 '(org-capture-templates
   (quote
    (("t" "Add new task" entry
      (file "~/AeroFS/org/tasks.org")
      "* TODO %?
  %u"))))
 '(org-default-notes-file "~/AeroFS/org/notes.org")
 '(org-directory "~/AeroFS/org")
 '(org-reverse-note-order t)
 '(projectile-completion-system (quote helm))
 '(projectile-enable-caching t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

  ;; Fullscreen
  (toggle-frame-fullscreen)
  )

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
