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
      straight-default-vc 'git)

;; Provies better defaults for emacs cache files
(use-package no-littering :demand t)

;; Saving files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Appearance

;; Turn off the alarm bell
(setq ring-bell-function #'ignore)

;; Display keystrokes in the echo area immediately
(setq echo-keystrokes 1e-6)

;; Don't blink the cursor on the opening paren when you insert a closing paren
(setq blink-matching-paren nil)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Prevent the cursor from blinking
(blink-cursor-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable scratch buffer message
(setq initial-scratch-message nil)

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
  ;; Bind ESC to minibuffer-keyboard-quit
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  (general-create-definer mars-map
    :states '(normal motion emacs))

  ;; Unbind SPC
  (mars-map "SPC" nil)

  ;; Dired
  (with-eval-after-load 'dired
    (mars-map :keymaps 'dired-mode-map "SPC" nil))

  (defvar mars-general-prefixes nil
    "Defines prefixes for each section")

  (defvar mars-general-prefixes
    '(("b" . "buffers")
      ("f" . "files")
      ("p" . "projects")
      ("e" . "eval")
      ("w" . "windows")
      ("h" . "help")
      ("i" . "ivy")
      ("g" . "git"))
    "Each car in the alist is the prefix key and the cdr defines the suffix of
mars-map/ function")

  ;; Defines general definers for each prefix in mars-general-prefixes.
  (eval `(progn
	   ,@(mapcar
	      (lambda (prefix)
		(let* ((section (cdr prefix))
		       (definer (intern (concat "mars-map/" section))))
		  `(general-create-definer ,definer
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
    "b" 'eval-buffer))

;; Manipulating emacs process

;; Restart emacs
(use-package restart-emacs :demand t)

;; Candidate selection

;; Select things in the minibuffer
(use-package swiper
  :straight (:host github :repo "abo-abo/swiper"
		   :files (:defaults (:exclude "counsel.el"))
		   :fork (:repo "qleguennec/swiper"))

  :config (ivy-mode 1)
  (mars-map "'" 'ivy-resume))

;; Remaps common emacs functions
(use-package counsel
  :demand t
  :straight
  (:host github :repo "abo-abo/swiper"
	 :fork (:repo "qleguennec/swiper")
	 :files ("counsel.el"))

  :config
  (counsel-mode 1)

  :general
  (mars-map/help
    "f" 'counsel-describe-function
    "v" 'counsel-describe-variable
    "k" 'counsel-descbinds)

  (mars-map/ivy
    "y" 'counsel-yank-pop
    "c" 'counsel-command-history))

;; Provide statistics for sorting/filtering
(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :demand t
  :config
  (ivy-prescient-mode 1))

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
    "s" 'magit-stage)
  (:keymaps 'magit-mode-map
	    "SPC" nil))

(use-package evil-magit :demand t)

;; File management

(use-package projectile :config (projectile-mode 1))

;; Editor features
(use-package counsel-projectile
  :straight (:host github :repo "ericdanan/counsel-projectile"
		   :fork (:repo "qleguennec/counsel-projectile"))

  :config
  (add-to-list 'projectile-globally-ignored-directories '("straight/" "node_modules/"))

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

  ;; Open magit when switching project
  (setq counsel-projectile-switch-project-action #'counsel-projectile-switch-project-action-vc)

  :general
  (mars-map
    "SPC SPC" 'counsel-projectile
    "?" 'counsel-projectile-ag)
  (mars-map/projects
    "p" 'counsel-projectile-switch-project))

;; Vim-like keybindings.
(use-package evil
  :demand t
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

;; Surround things.
(use-package evil-surround
  :demand t
  :config (global-evil-surround-mode 1))

;; Expand selected region at point.
(use-package expand-region
  :general
  (:keymaps 'evil-visual-state-map
	    "v" 'er/expand-region
	    "V" 'er/contract-region))

;; Simpler lisp editing

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode))

(use-package lispyville
  :hook (emacs-lisp-mode . lispyville-mode)
  :config (lispyville-set-key-theme '(slurp/barf-lispy)))

(use-package parinfer
  :disabled
  :config (setq parinfer-extensions '(defaults pretty-parens evil lispy))
  :hook (emacs-lisp-mode . parinfer-mode))

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
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-scope 'buffer)
  (setq evil-snipe-repeat-scope 'buffer))

;; Language packages

;; javascript
(use-package rjsx-mode
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

;; UI

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
  :config (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))

(use-package rainbow-delimiters
  :demand t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Font
(setq mars-font "Hack")
(setq mars-font-height 105)
(set-face-attribute 'default nil :family mars-font :height mars-font-height)
