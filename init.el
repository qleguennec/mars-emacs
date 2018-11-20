;;; -*- lexical-binding: t -*-

(defvar mars-workspace "~/wp"
  "The directory where projects are usually stored.
Every directory in mars-workspace is added to projectile-known-projects,
even if it does not countain a vcs subdir.")

;; Customs

;; Toggles
(defvar mars-ivy-use-preview t
  "If non-nil, counsel-projectile command will preview the selected buffer/file in the window where counsel-projectile was executed.")

;; Do not load package.el
(setq package-enable-at-startup nil)

;; Don't ask for confirmation for .dir-locals
(advice-add 'risky-local-variable-p :override #'ignore)

;; Save customs in a seperate file
(setq custom-file (expand-file-name
                   (format "custom-%d-%d.el" (emacs-pid) (random))
                   temporary-file-directory))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t
      use-package-always-defer t
      use-package-verbose t
      straight-default-vc 'git)

;; Custom org install
(require 'subr-x)

;; Requires the git package
(use-package git
  :demand t)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; Provies better defaults for emacs cache files
(use-package no-littering :demand t)

;; Saving files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Appearance

(setq

;; Turn off the alarm bell
 ring-bell-function #'ignore

 ;; Display keystrokes in the echo area immediately
 echo-keystrokes 1e-6

 ;; Don't blink the cursor on the opening paren when you insert a closing paren
 blink-matching-paren nil)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Prevent the cursor from blinking
(blink-cursor-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Highlight current line.
(hl-line-mode 1)

(setq
 ;; Disable startup screen
 inhibit-startup-screen t

 ;; Disable scratch buffer message
 initial-scratch-message nil)

;; el-patch

;; Package `el-patch' provides a way to override the definition of an
;; internal function from another package by providing an s-expression
;; based diff which can later be validated to ensure that the upstream
;; definition has not changed.
(use-package el-patch
  :straight (:host github :repo "raxod502/el-patch" :branch "develop")
  :demand t)

;; Keybindings

;; Set-up general early so we can later use it in use-package.
(use-package general
  :demand t

  :config
  (general-create-definer mars-map
    :states '(normal motion emacs))

  ;; Unbind SPC
  (mars-map "SPC" nil)

  (defvar mars-general-prefixes nil
    "Each car in the alist is the prefix key and the cdr defines the suffix of
mars-map/ function")

  (setq mars-general-prefixes
	'(("b" . "buffers")
	  ("f" . "files")
	  ("p" . "projects")
	  ("e" . "eval")
	  ("w" . "windows")
	  ("h" . "help")
	  ("i" . "ivy")
	  ("g" . "git")
	  ("o" . "org")))

  ;; Defines general definers for each prefix in mars-general-prefixes.
  (eval `(progn
	   ,@(mapcar
	      (lambda (prefix)
		(let* ((section (cdr prefix))
		       (definer (intern (concat "mars-map/" section))))
		  `(general-create-definer ,definer
		     :keymaps 'override
		     :states '(normal motion emacs)
		     :prefix ,(concat "SPC " (car prefix)))))
	      mars-general-prefixes)))

  (mars-map/buffers
    "s" 'save-buffer
    "p" 'previous-buffer
    "n" 'next-buffer
    "k" 'kill-buffer
    "b" 'counsel-ibuffer)

  (mars-map/eval
    "b" 'eval-buffer
    "r" 'eval-region
    "f" 'eval-defun
    "e" 'eval-expression))

;; Org mode
(use-feature org
  :init
  (use-package org
    :config
    (setq org-directory "~/org")

    :general
    (mars-map/org
      "c" 'org-capture))

  ;; Per-project org things
  (use-package org-projectile
    :config
    (org-projectile-per-project)
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; Prettier org
(use-package org-bullets
  :demand t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Restart emacs
(use-package restart-emacs :demand t)

;; Candidate selection

;; Select things in the minibuffer
(use-feature ivy
  :init
  (use-package ivy
    :config
    (ivy-mode 1)

    :general
    (mars-map "'" 'ivy-resume)
  
    (mars-map/help
      "f" 'counsel-describe-function
      "v" 'counsel-describe-variable
      "k" 'counsel-descbinds)

    (mars-map/ivy
      "y" 'counsel-yank-pop
      "c" 'counsel-command-history
      "o" 'counsel-mark-ring))

  (use-package ivy-hydra)

  ;; Provide statistics for sorting/filtering
  (use-package prescient
    :config
    (prescient-persist-mode 1))

  (use-package ivy-prescient
    :demand t
    :config
    (ivy-prescient-mode 1)))

;; Displays helpful documentation
(use-package helpful
  :demand t
  :after counsel
  :config
  (setq counsel-describe-function-function #'helpful-function)
  counsel-describe-variable-function #'helpful-variable

  :general
  (mars-map/help
    "." 'helpful-at-point))

;; VC
(use-package magit
  :config
  (global-magit-file-mode 1)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

  :general
  (mars-map/git
    "g" 'magit-status
    "s" 'magit-stage))

;; File management

(use-feature projectile
  :init
  (use-package projectile :config (projectile-mode 1))

  ;; Editor features
  (use-package counsel-projectile
    :straight (:host github :repo "ericdanan/counsel-projectile"
		     :fork (:repo "qleguennec/counsel-projectile"))

    :config
    (setq projectile-globally-ignored-directories
	  (append projectile-globally-unignored-directories '("straight" "node_modules")))

    (defun mars-projectile-refresh-projects ()
      "Clear projectile known projects and add to known projects directories in mars-workspace
(even if they are not under vcs), any vcs directory in HOME, and straight repos"
      (interactive)
      (projectile-clear-known-projects)
      (setq projectile-known-projects
	    (append
	     (directory-files mars-workspace t directory-files-no-dot-files-regexp t)
	     (cl-remove-if
	      (lambda (dir)
		(eq 'none (projectile-project-vcs dir)))
	      (directory-files "~/" t directory-files-no-dot-files-regexp t))
	     (directory-files (concat user-emacs-directory "/straight/repos") t directory-files-no-dot-files-regexp t)))
      (projectile-save-known-projects))

    (unless projectile-known-projects (mars-projectile-refresh-projects))

    :general
    (mars-map
      "SPC SPC" 'counsel-projectile
      "?" 'counsel-projectile-ag)
    (mars-map/projects
      "p" 'counsel-projectile-switch-project)))

;; Vim-like keybindings.
(use-feature evil
  :init
  (use-package evil
    :demand t
    :init
    ;; Required for evil-collection
    (setq evil-want-integration t
	  evil-want-keybinding nil)

    :config
    (evil-mode 1)

    ;; Unbind SPC in motion-state-map
    ;; See https://stackoverflow.com/questions/33061926/emacs-evil-space-as-a-prefix-key-in-motion-state#33408565
    (define-key evil-motion-state-map " " nil)

    :general
    ;; Remaps evil-search-forward to swiper
    (mars-map
      [remap evil-search-forward] 'swiper)

    (mars-map
      ;; Window motion
      "C-h" 'evil-window-left
      "C-l" 'evil-window-right
      "C-j" 'evil-window-down
      "C-k" 'evil-window-up)

    (mars-map/windows
      ;; Window motion
      "h" 'evil-window-left
      "l" 'evil-window-right
      "j" 'evil-window-down
      "k" 'evil-window-up

      ;; Window manipulation
      "v" (lambda () (interactive) (evil-window-vsplit) (other-window 1))
      "s" (lambda () (interactive) (evil-window-split) (other-window 1))
      "q" 'evil-window-delete))

  (use-package evil-collection
    :demand t
    :config
    (evil-collection-init))

  (use-package evil-magit
    :demand t))

(use-package flycheck
  :init
  (global-flycheck-mode 1)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

;; Surround things.
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  
  :general
  (mars-map
    "g d" 'dumb-jump-go
    "g D" 'dumb-jump-go-other-window))

(use-feature save-place
  :init
  (save-place-mode 1))

(use-feature subword
  :init
  (global-subword-mode 1))

(use-feature autorevert
  :defer 2
  :config
  (setq auto-revert-interval 1
	revert-without-query '(".*"))
  (global-auto-revert-mode 1))

(use-package evil-surround
  :demand t
  :config (global-evil-surround-mode 1))

(use-package expand-region
  :general
  (:keymaps 'evil-visual-state-map
	    "v" 'er/expand-region
	    "V" 'er/contract-region))

;; Simpler lisp editing

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-strict-mode))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :config (lispyville-set-key-theme '(slurp/barf-lispy
				      text-objects
				      lispyville-prettify
				      atom-motions)))

;; Completion
(use-package company
  :demand t
  :config (global-company-mode 1))

;; Jump on things
(use-package avy
  :config (avy-setup-default)

  :general
  (mars-map
    "J" 'avy-goto-line-below
    "K" 'avy-goto-line-above))

(use-package evil-snipe
  :demand t
  :init
  (evil-snipe-override-mode 1)
  (evil-snipe-mode 1)
  (setq evil-snipe-scope 'buffer)
  (setq evil-snipe-repeat-scope 'buffer))

(use-package smartparens
  :demand t
  :init
  (smartparens-global-strict-mode 1)
  :config
  (require 'smartparens-config))

;; Language packages

;; javascript
(use-feature javascript
  :init
  (setq js-indent-level 2)

  (use-package rjsx-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    :config
    (flycheck-select-checker 'javascript-eslint))

  (use-package prettier-js
    :init (add-hook 'rjsx-mode-hook 'prettier-js-mode)
    :config
    (setq prettier-js-command "prettier_d")))

;; lsp
(use-feature lsp
  :init
  (use-package lsp-mode
    :config
    (add-hook 'before-save-hook (lambda () (when (eq major-mode 'lsp-mode)
					     (lsp-format-buffer)))))

  (use-package lsp-ui
    :init
    (add-hook 'lsp-mode-hook #'lsp-ui-mode))

  (use-package company-lsp)

  (use-package lsp-intellij
    :demand t
    :init
    (add-hook 'java-mode-hook #'lsp-intellij-enable)
    :config
    (setq lsp-intellij-server-port 4224)
    (push 'company-lsp company-backends)
    (setq company-lsp-enable-snippet t
	  company-lsp-cache-candidates t)
    (add-hook 'before-save-hook #'lsp-format-buffer)))

;; UI

;; Use ediff on the same window
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Highlight lisp expressions
(show-paren-mode 1)

(setq show-paren-style 'expression)

(use-package doom-themes
  :demand t
  :config (load-theme 'doom-city-lights 'confirm))

(use-package doom-modeline
  :demand t
  :config (doom-modeline-init))

(use-package centered-cursor-mode
  :demand t
  :config (global-centered-cursor-mode 1))

(use-package solaire-mode
  :demand t
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'magit-mode-hook #'solaire-mode))

(use-package rainbow-delimiters
  :demand t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package origami
  :disabled
  :init (global-origami-mode 1))

;; Font
(setq mars-font "Hack")
(setq mars-font-height 105)
(set-face-attribute 'default nil :family mars-font :height mars-font-height)

;; Open init.el on startup

(find-file user-init-file)
