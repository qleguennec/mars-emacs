;;; -*- lexical-binding: t -*-

(defvar mars-workspace "~/wp"
  "The directory where projects are usually stored.
Every directory in mars-workspace is added to projectile-known-projects,
even if it does not countain a vcs subdir.")

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

(defun mars-reload-init-file ()
  "Reload init.el, marking it as a straight transaction."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t
      use-package-always-defer t
      use-package-verbose t
      straight-default-vc 'git)

;; Enable checking for system packages
(use-package use-package-ensure-system-package
  :demand t)

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
	  ("o" . "org")
	  ("a" . "applications")))

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
    "b" 'counsel-ibuffer
    "e" (lambda ()
	  (interactive)
	  (switch-to-buffer "*el scratch*")
	  (emacs-lisp-mode 1)))

  (mars-map/eval
    "b" 'eval-buffer
    "r" 'eval-region
    "f" 'eval-defun
    "e" 'eval-expression))

;; Enforces popup rules
 (use-package shackle
   :demand t
   :config
   (shackle-mode 1)
   ;; No unwanted splitting
   (setq
    shackle-select-reused-windows t
    shackle-inhibit-window-quit-on-same-windows t))

;; Org mode
(use-feature org
  :init
  (use-package org
    :defer 3
    :config
    (setq org-directory "~/org")

    :general
    (mars-map/org
      "c" 'org-capture))

  ;; Per-project org things
  (use-package org-projectile
    :after org
    :config
    (org-projectile-per-project)
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; Prettier org
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Restart emacs
(use-package restart-emacs)

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
  :init
  (setq counsel-describe-function-function #'helpful-function
	counsel-describe-variable-function #'helpful-variable)
  (push '(".*\\*helpful.*\\*" :regexp t :align right :size 0.3) shackle-rules)

  :general
  (mars-map/help
    "." 'helpful-at-point))

;; VC
(use-package magit
  :config
  (global-magit-file-mode 1)
  ;; Auto commits in wip refs
  (magit-wip-mode 1)
  (setq
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   ;; Always save everything before opening a magit buffer
   magit-save-repository-buffers 'dontask)

  ;; Refresh after a save.
  (add-hook 'after-save-hook #'magit-refresh)

  ;; Pops magit in another window
  (push '("magit: .*" :regexp t :align right :size 0.4) shackle-rules)

  :general
  (mars-map/git
    "g" 'magit-status
    "s" 'magit-stage))

;; Github things
(use-package magithub
  :disabled
  :demand t
  :config
  (magithub-feature-autoinject t)
  (setq magithub-dir mars-workspace))

;; Disable built in emacs vc as we have magit for that
(use-feature vc-hooks
  :config
  (setq vc-handled-backends nil))

;; Project management
(use-feature projectile
  :init
  (use-package projectile :config (projectile-mode 1))

  ;; Editor features
  (use-package counsel-projectile
    :straight (:host github :repo "ericdanan/counsel-projectile")
    
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

;; Editor features

(setq
 ;; Don't break lines
 truncate-lines t
 ;; Don't ask for confirmation for .dir-locals
 enable-local-variables :all)

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
    (mars-map
      ;; Remaps evil-search-forward to swiper
      [remap evil-search-forward] 'swiper)

    (mars-map
      ;; Window resizing
      "C-h" 'shrink-window-horizontally
      "C-l" 'enlarge-window-horizontally
      "C-j" 'shrink-window
      "C-k" 'enlarge-window)

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
    :after magit
    :demand t)

  (use-package smerge-mode
    :after magit
    :init
    (defhydra unpackaged/smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue))
    :hook (magit-diff-visit-file . (lambda ()
                                     (when smerge-mode
                                       (unpackaged/smerge-hydra/body))))))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode 1)
  (push '(".*\\*undo-tree\\*.*" :regexp t :align left :size 0.1) shackle-rules)

  :general
  (mars-map
    "U" 'undo-tree-visualize
    "u" 'undo-tree-undo))

(use-package flycheck
  :init
  (global-flycheck-mode 1)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

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

;; Save things when switching buffers and other things
(use-package super-save
  :demand t
  :config
  (super-save-mode 1))

(use-feature subword
  :init
  (global-subword-mode 1))

(use-feature autorevert
  :defer 2
  :config
  (setq auto-revert-interval 1
	revert-without-query '(".*"))
  (global-auto-revert-mode 1))

;; Surround things.
(use-package evil-surround
  :demand t
  :config (global-evil-surround-mode 1))

(use-package expand-region
  :general
  (:keymaps 'evil-visual-state-map
   "v" 'er/expand-region
   "V" 'er/contract-region))

;; Completion
(use-feature completion
  :init
  (use-package company
    :defer 3
    :config
    ;; Always display the entire suggestion list onscreen, placing it
    ;; above the cursor if necessary.
    (setq company-tooltip-minimum company-tooltip-limit)
    (global-company-mode 1))

  (use-package company-prescient
    :demand t
    :after company
    :config (company-prescient-mode 1)))

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

(use-feature lisp
  :init
  (use-package lispy
    :hook ((emacs-lisp-mode clojure-mode cider-mode) . lispy-mode)
    :config
    (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)
    (add-hook 'lispy-mode-hook #'turn-off-smartparens-strict-mode))

  (use-package lispyville
    :hook (lispy-mode . lispyville-mode)
    :config
    (setq lispyville-motions-put-into-special t
	  lispyville-commands-put-into-special t)
    (lispyville-set-key-theme '(slurp/barf-lispy
				text-objects
				lispyville-prettify
				escape))
    :general
    (:keymaps 'lispyville-mode-map
     :states 'normal
     "(" 'lispyville-backward-up-list
     ")" 'lispyville-up-list))

  :init/el-patch
  ;; From https://github.com/raxod502/radian/blob/dc22d0524481b45dd3097bf5d9d4f2cd7ad3bad9/radian-emacs/radian-elisp.el#L21-L116
  ;; Fix the indentation of keyword lists in Emacs Lisp. See [1] and [2].
  ;;
  ;; Before:
  ;;  (:foo bar
  ;;        :baz quux)
  ;;
  ;; After:
  ;;  (:foo bar
  ;;   :bar quux)
  ;;
  ;; [1]: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
  ;; [2]: http://emacs.stackexchange.com/q/10230/12534
  (defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (el-patch-let (($cond (and (elt state 2)
			       (el-patch-wrap 1 1
				 (or (not (looking-at "\\sw\\|\\s_"))
				     (looking-at ":")))))
		   ($then (progn
			    (if (not (> (save-excursion (forward-line 1) (point))
					calculate-lisp-indent-last-sexp))
				(progn (goto-char calculate-lisp-indent-last-sexp)
				       (beginning-of-line)
				       (parse-partial-sexp (point)
							   calculate-lisp-indent-last-sexp 0 t)))
			    ;; Indent under the list or under the first sexp on the same
			    ;; line as calculate-lisp-indent-last-sexp.  Note that first
			    ;; thing on that line has to be complete sexp since we are
			    ;; inside the innermost containing sexp.
			    (backward-prefix-chars)
			    (current-column)))
		   ($else (let ((function (buffer-substring (point)
							    (progn (forward-sexp 1) (point))))
				method)
			    (setq method (or (function-get (intern-soft function)
							   'lisp-indent-function)
					     (get (intern-soft function) 'lisp-indent-hook)))
			    (cond ((or (eq method 'defun)
				       (and (null method)
					    (> (length function) 3)
					    (string-match "\\`def" function)))
				   (lisp-indent-defform state indent-point))
				  ((integerp method)
				   (lisp-indent-specform method state
							 indent-point normal-indent))
				  (method
				   (funcall method indent-point state))))))
      (let ((normal-indent (current-column))
	    (el-patch-add
	      (orig-point (point))))
	(goto-char (1+ (elt state 1)))
	(parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
	(el-patch-swap
	  (if $cond
	      ;; car of form doesn't seem to be a symbol
	      $then
	    $else)
	  (cond
	   ;; car of form doesn't seem to be a symbol, or is a keyword
	   ($cond $then)
	   ((and (save-excursion
		   (goto-char indent-point)
		   (skip-syntax-forward " ")
		   (not (looking-at ":")))
		 (save-excursion
		   (goto-char orig-point)
		   (looking-at ":")))
	    (save-excursion
	      (goto-char (+ 2 (elt state 1)))
	      (current-column)))
	   (t $else)))))))

(use-feature javascript
  :init
  (setq js-indent-level 2)

  (use-package rjsx-mode
    :init
    (defun mars/eslint-locate ()
      (let* ((root (locate-dominating-file
		    (or (buffer-file-name) default-directory)
		    "node_modules"))
	     (eslint (and root
			  (expand-file-name "node_modules/eslint/bin/eslint.js"
					    root))))
	(when (and eslint (file-executable-p eslint))
	  (setq-local flycheck-javascript-eslint-executable eslint))))

    (add-hook 'rjsx-mode #'mars/eslint-locate)

    :config
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)) 
    (flycheck-add-mode 'javascript-eslint 'rjsx-mode))
  

  (use-package prettier-js
    :init (add-hook 'rjsx-mode-hook #'prettier-js-mode)
    :config
    (setq prettier-js-command "prettier_d")
    (setq prettier-js-args '("--trailing-comma" "es5"
			     "--print-width" "120"
			     "--single-quote" "true"
			     "--tab-width" "2"
			     "--use-tabs" "false"))

  
    ;; Package `tern' provides a static code analyzer for JavaScript. This
    ;; includes ElDoc and jump-to-definition out of the box.
    (use-package tern
      :demand t
      :after rjsx-mode
      :config
      (add-hook 'js2-mode-hook #'tern-mode))
    
    ;; Package `company-tern' provides a Company backend which uses Tern.
    (use-package company-tern
      :demand t
      :after (:all rjsx-mode company tern)
      :config
      (add-hook 'js2-mode-hook
		(lambda () (setq-local company-backends
				       (push 'company-tern company-backends)))))))

(use-package yaml-mode
  :config (setq yaml-indent-offset 4))

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
    :commands 'lsp-intellij-enable
    :init
    (add-hook 'java-mode-hook #'lsp-intellij-enable)
    :config
    (setq lsp-intellij-server-port 4224)
    (push 'company-lsp company-backends)
    (setq company-lsp-enable-snippet t
	  company-lsp-cache-candidates t)
    (add-hook 'before-save-hook #'lsp-format-buffer)))

(use-feature clojure
  :init
  (use-package cider
    :config
    (setq clojure-indent-style :align-arguments)

    :general
    (:keymaps 'cider-repl-mode-map
     :states '(normal insert)
     "<up>" 'cider-repl-backward-input
     "<down>" 'cider-repl-forward-input))

  (use-package clj-refactor
    :hook (clojure-mode-hook . clj-refactor-mode)

    :config
    ;; Automatically sort project dependencies after changing them.
    (setq cljr-auto-sort-project-dependencies t)))

;; UI

;; Use ediff on the same window
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Highlight lisp expressions
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

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
  :commands rainbow-delimiters-mode
  :hook ((clojure-mode cider-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))
  :general
  (mars-map/windows
    "w" 'ace-window))

;; Applications

;; Shell

(use-feature comint
  :general
  (:keymaps 'comint-mode-map
   :states '(normal insert)
   "C-j" nil
   "C-k" nil
   "<up>" 'comint-previous-input
   "<down>" 'comint-next-input))

(use-feature eshell
  :config
  (push '(eshell-mode :popup t) shackle-rules)
  (setq
   ;; Send inpupt to suprocesses
   eshell-send-direct-to-subprocesses nil
   eshell-buffer-maximum-lines 5000)

  (defun mars-eshell-new-buffer ()
    "Open a new eshell buffer if one already exists"
    (interactive)
    (let ((eshell-buffers-count
	   (thread-last (buffer-list)
	     (seq-filter (lambda (buffer) (string-match ".*\\*eshell.*\\*"
							(buffer-name buffer))))
	     (length))))
      (unless (zerop eshell-buffers-count)
	(setq eshell-buffer-name
	      (format "*eshell-%d*" eshell-buffers-count))))
    (eshell))

  :general
  (mars-map/applications
    "e" 'mars-eshell-new-buffer))

;; Mail reader
(use-package mu4e
  :ensure-system-package mu
  :defer 5
  :commands 'mu4e
  :init (setq mail-user-agent 'mu4e-user-agent)
  :config
  (setq
   mu4e-completing-read-function 'ivy-completing-read
   mu4e-maildir "~/.mail"
   mu4e-get-mail-command "mbsync -a"
   mu4e-sent-messages-behavior 'delete
   mu4e-contexts
   `(,(make-mu4e-context
       :name "Gmail"
       :match-func (lambda (msg)
		     (when msg
		       (string-prefix-p "Gmail"
					(mu4e-message-field msg :maildir))))
       :vars '((mu4e-trash-folder . "/gmail/Trash")
	       (mu4e-sent-folder . "/gmail/Sent")
	       (mu4e-drafts-folder . "/gmail/Drafts")
	       (user-mail-address . "quentin.leguennec1@gmail.com")
	       (user-full-name . "Quentin Le Guennec")
	       (smtpmail-smtp-user . "quentin.leguennec1")
	       (smtpmail-default-smtp-server . "smtp.gmail.com")
	       (smtpmail-smtp-server . "smtp.gmail.com")
	       (smtpmail-smtp-service . 587)))))
  :general
  (mars-map/applications "m" 'mu4e)
  (:keymaps 'mu4e-headers-mode-map
   :states 'normal
   "g r" 'mu4e-update-mail-and-index))

;; Font
(setq mars-font "Hack")
(setq mars-font-height 105)
(set-face-attribute 'default nil :family mars-font :height mars-font-height)

;; Starts emacs server
(server-start)

;; Open init.el on startup
(find-file user-init-file)
