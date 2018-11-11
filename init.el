;;; -*- lexical-binding: t -*-

(defvar mars-workspace "~/wp"
  "The directory where projects are usually stored.
Every directory in mars-workspace is added to projectile-known-projects,
even if it does not countain a vcs subdir.")

;; Customs

;; Toggles
(defvar mars-ivy-use-preview t
  "If non-nil, counsel-projectile command will preview the selected buffer/file in the window where counsel-projectile was executed.")

;; TODO Needed for development, remove this when this repo becomes the default config
(setq user-init-file (or load-file-name (buffer-file-name))
      user-emacs-directory (file-name-directory user-init-file))

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
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq straight-default-vc 'git)

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

  (setq mars-general-prefixes
  	'(("b" . "buffers")
  	  ("f" . "files")
  	  ("p" . "projects")
  	  ("e" . "eval")
	  ("w" . "windows")
	  ("h" . "help")))

  ;; Defines general definers for each prefix in mars-general-prefixes.
  (eval
   `(progn ,@(mapcar (lambda (prefix)
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
(use-package ivy)

;; Remaps common emacs functions
(use-package counsel
  :demand t

  :config
  (counsel-mode 1)

  :general
  (mars-map/help
   "f" 'counsel-describe-function
   "v" 'counsel-describe-variable
   "k" 'counsel-descbinds))

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
  (setq counsel-describe-variable-function #'helpful-variable)

  :general
  (mars-map/help
    "." 'helpful-at-point))

;; VC
(use-package magit
  :config
  (global-magit-file-mode 1)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

  :general
  (mars-map/projects "g" 'magit-status)
  (:keymaps 'magit-mode-map
	    "SPC" nil))

(use-package evil-magit :demand t)

;; File management

(use-package projectile :config (projectile-mode 1))

(use-package ivy
  :straight (:host github :repo "abo-abo/swiper"
		   :fork (:repo "qleguennec/swiper"))
  :config (ivy-mode 1))

(use-package counsel :config (counsel-mode 1))

(use-package counsel-projectile
  :straight (:host github :repo "ericdanan/counsel-projectile"
		   :fork (:repo "qleguennec/counsel-projectile"))

  :config
  (add-to-list 'projectile-globally-ignored-directories "straight/")

  (defun mars-projectile-refresh-projects ()
    "Clear projectile known projects and add to known projects directories in mars-workspace
(even if they are not under vcs), any vcs directory in HOME, and straight repos"
    (interactive)
    (projectile-clear-known-projects)
    (setq projectile-known-projects
	  (append
	   (directory-files mars-workspace t directory-files-no-dot-files-regexp t)
	   (cl-remove-if (lambda (dir)
			   (eq 'none (projectile-project-vcs dir)))
			 (directory-files "~/" t directory-files-no-dot-files-regexp t))
	   (directory-files (concat user-emacs-directory "/straight/repos") t directory-files-no-dot-files-regexp t)))
    (projectile-save-known-projects))

  (setq counsel-projectile-switch-project-action #'counsel-projectile-switch-project-action-vc)

  :general
  (mars-map
    "SPC SPC" 'counsel-projectile
    "?" 'counsel-projectile-ag)
  (mars-map/projects
    "p" 'counsel-projectile-switch-project))

;;  

;; Editor features

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

;; Auto-pairs parens and other things.
(use-package smartparens
  :demand t

  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

;; Completion
(use-package company
  :demand t
  :config (global-company-mode 1)

  (defun mars-company-select-with-ivy ()
    "Select a candidate for autocompletion with ivy."
    (interactive)
    (ivy-read "Select candidate: " company-candidates
	      :action (lambda (selection)
			(company-set-selection selection)
			(company-complete-selection))))
  :general
  (:keymaps 'company-active-map
	    ;; Disabled for now, will be mars-company-select-with-ivy in the future
   "SPC" nil))

;; UI
(use-package doom-themes
  :demand t
  :config (load-theme 'doom-city-lights 'confirm))

(use-package doom-modeline
  :demand t
  :config (doom-modeline-init))

;; Font
(setq mars-font "Hack")
(setq mars-font-height 105)
(set-face-attribute 'default nil :family mars-font :height mars-font-height)
