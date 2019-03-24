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

(defun mars/yank-current-file ()
  "Yanks the current edited file path"
  (interactive)
  (kill-new buffer-file-name))

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

(defmacro mars/add-to-list (list &rest args)
  "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (dolist (item args)
    (add-to-list list item))
  list)

(defun mars/set-pretty-symbols (mode symbols)
  ;; TODO fix it. should be a macro.
  "Set symbols SYMBOLS for quoted mode MODE."
  (let ((alist (mapcar (lambda (symbol)
			 `(,(car symbol) . (string-to-char (cdr symbol))))
		       symbols)))
    (add-hook mode (lambda ()
		     (interactive)
		     (setq-local prettify-symbols-alist alist)))))

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

  (general-create-definer mars-leader-map
    :states '(normal motion emacs)
    :prefix "m")

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
	  ("o" . "org")
	  ("a" . "applications")
	  ("f" . "frames")))

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

  (general-create-definer mars/map
    :keymaps 'override
    :states '(normal motion emacs))

  (mars/map
    "C-s" 'save-buffer)

  (mars-map/buffers
    "s" 'save-buffer
    "p" 'previous-buffer
    "n" 'next-buffer
    "k" 'kill-buffer
    "b" 'ivy-switch-buffer
    "e" (lambda ()
	  (interactive)
	  (switch-to-buffer "*el scratch*")
	  (emacs-lisp-mode 1)))

  (mars-map/eval
    "b" 'eval-buffer
    "r" 'eval-region
    "f" 'eval-defun
    "e" 'eval-expression))

;; Org mode
(use-feature org
  :init
  (use-package org
    :demand t
    :config

    (defun mars/open-gtd-file ()
      "Open gtd file."
      (interactive)
      (find-file org-default-notes-file))

    (defun mars/org-insert-heading ()
      "Inserts a new heading and switches to insert state."
      (interactive)
      (org-insert-heading-respect-content)
      (evil-insert 1))

    (setq
     org-directory "~/org"
     org-default-notes-file (expand-file-name "gtd.org.gpg" org-directory)
     org-agenda-files (list org-default-notes-file)
     org-startup-indented t
     org-pretty-entities t
     org-agenda-block-separator ""
     org-fontify-whole-heading-line t
     org-fontify-done-headline t
     org-fontify-quote-and-verse-blocks t
     org-reverse-note-order t
     org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "CANCELLED")))

    (setq org-capture-templates
	  '(("j" "Journal" entry (file+olp+datetree "" "Journal")
             "* %?"
	     :empty-lines 1)

	    ("t" "Todo" entry (file+headline "" "Inbox")
             "* TODO "
	     :empty-lines 1)

	    ("t" "Todo" entry (file+headline "" "Courses")
             "* TODO "
	     :empty-lines 1)))

    (setq org-global-properties
	  '(("Effort_ALL" . "1 2 3 5 7")))

    :general
    (mars-map/org
      "c" 'counsel-org-capture
      "t" 'mars/open-gtd-file)

    (mars-leader-map
      :keymaps 'org-mode-map
      "t" (lambda () (interactive) (org-todo "TODO"))
      "w" (lambda () (interactive) (org-todo "WAITING"))
      "o" (lambda () (interactive) (org-todo "DOING"))
      "d" (lambda () (interactive) (org-todo "DONE"))
      "c" (lambda () (interactive) (org-todo "CANCELLED")))

    (:keymaps 'org-mode-map
     :states '(normal visual)
     "RET" 'mars/org-insert-heading
     "<up>" 'org-metaup
     "<down>" 'org-metadown
     "<left>" 'org-shiftleft
     "<right>" 'org-shiftright

     "C-j" nil
     "C-k" nil

     "?" 'counsel-org-goto

     [remap evil-shift-left] 'org-metaleft
     [remap evil-shift-right] 'org-metaright)

    (:keymaps 'org-agenda-mode-map
     :states '(normal visual emacs)
     "RET" 'org-agenda-switch-to
     "j" 'org-agenda-next-line
     "k" 'org-agenda-previous-line))

  ;; Per-project org things
  (use-package org-projectile
    :after org
    :config
    (org-projectile-per-project)
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))

  (use-package org-trello)

  ;; Prettier org
  (use-package org-bullets
    :init (add-hook 'org-mode-hook #'org-bullets-mode)))


(use-package restart-emacs)

;; Edit root file
(use-package sudo-edit)

;; Candidate selection

;; Select things in the minibuffer
(use-feature ivy
  :init
  (use-package ivy
    :config
    (ivy-mode 1)

    (defun mars/relative-filename-without-extension (filename)
      (interactive)
      (file-name-sans-extension
       (file-relative-name
	(expand-file-name filename (projectile-project-root)))))

    (defun mars/insert-relative-filename-without-extension (filename)
      (interactive)
      (insert
       (mars/relative-filename-without-extension filename)))

    (ivy-add-actions
     #'counsel-find-file
     '(("p" mars/insert-relative-filename-without-extension "insert relative")))

    (ivy-add-actions
     #'counsel-projectile-find-file
     '(("p" mars/insert-relative-filename-without-extension "insert relative")))

    (ivy-add-actions
     #'counsel-projectile
     '(("p" mars/insert-relative-filename-without-extension "insert relative")))


    :general
    (mars-map
      "'" 'ivy-resume
      "M-x" 'counsel-M-x)

    (mars-map/help
      "f" 'counsel-describe-function
      "v" 'counsel-describe-variable
      "k" 'counsel-descbinds)

    (mars-map/ivy
      "y" 'counsel-yank-pop
      "c" 'counsel-command-history
      "o" 'counsel-mark-ring
      "s" 'counsel-shell-history))

  (use-package ivy-hydra)

  ;; Provide statistics for sorting/filtering
  (use-package prescient
    :config (prescient-persist-mode 1))

  (use-package ivy-prescient
    :demand t
    :config (ivy-prescient-mode 1))

  (use-package ivy-filthy-rich
    :straight (:host github :repo "casouri/ivy-filthy-rich")
    :demand t
    :config (ivy-filthy-rich-mode 1)))

;; Displays helpful documentation
(use-package helpful
  :init
  (setq counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable)

  :general
  (mars-map/help
    "." 'helpful-at-point))

;; Displays demos
(use-package elisp-demos
  :init (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

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

  :general
  (:keymaps 'magit-mode-map
   :states 'normal
   "<escape>" nil
   "q" 'magit-mode-bury-buffer))

;; git forges
(use-package forge
  :disabled
  :after magit)

;; Create URLs for files and commits in GitHub/Bitbucket/GitLab/... repositories
(use-package git-link)

(use-package git-auto-commit-mode)

;; Disable built in emacs vc as we have magit for that
(use-feature vc-hooks
  :config
  (setq vc-handled-backends nil))

;; Project management
(use-feature projectile
  :init
  (use-package projectile :config (projectile-mode 1))

  (use-package counsel-projectile
    :straight (:host github :repo "ericdanan/counsel-projectile")
    :demand t

    :config
    (setq projectile-globally-ignored-directories
	  (append projectile-globally-unignored-directories '("straight" "node_modules")))

    (defun mars/projectile-refresh-projects ()
      "Clear projectile known projects and add to known projects directories in mars-workspace
(even if they are not under vcs), any vcs directory in HOME, and straight repos"
      (interactive)
      (projectile-clear-known-projects)
      (setq projectile-known-projects
	    (--map
	     (concat (s-replace (expand-file-name "~") "~" it) "/")
	     (append
	      (directory-files mars-workspace t directory-files-no-dot-files-regexp t)
	      (cl-remove-if
	       (lambda (dir)
		 (eq 'none (projectile-project-vcs dir)))
	       (directory-files "~/" t directory-files-no-dot-files-regexp t))
	      (directory-files (concat user-emacs-directory "/straight/repos") t directory-files-no-dot-files-regexp t))))
      (projectile-save-known-projects))

    (unless projectile-known-projects (mars/projectile-refresh-projects))

    :general
    (mars-map
      "SPC SPC" 'counsel-projectile
      "?" 'counsel-projectile-git-grep)
    (mars-map/projects
      "p" 'counsel-projectile-switch-project
      "b" 'counsel-projectile-switch-to-buffer)))

(use-package buffer-expose
  :straight (:host github :repo "clemera/buffer-expose")
  :demand t
  :config
  (buffer-expose-mode 1)

  :general
  (:keymaps 'buffer-expose-grid-map
   "h" 'buffer-expose-left-window
   "j" 'buffer-expose-down-window
   "k" 'buffer-expose-up-window
   "l" 'buffer-expose-right-window
   "(" 'buffer-expose-prev-page
   ")" 'buffer-expose-next-page))

;; Editor features
(define-minor-mode mars/before-save-mode
  "Minor mode to automatically fix whitespace on save.
If enabled, then saving the buffer deletes all trailing
whitespace and ensures that the file ends with exactly one
newline."
  nil nil nil
  (if mars/before-save-mode
      (progn
        (setq require-final-newline t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local))
    (setq require-final-newline nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

(define-globalized-minor-mode mars/before-save-global-mode
  mars/before-save-mode mars/before-save-mode)

(mars/before-save-global-mode 1)

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
    ;; Required for ollection
    (setq evil-want-integration t
	  evil-want-keybinding nil)

    :config
    (evil-mode 1)

    ;; Unbind SPC in motion-state-map
    ;; See https://stackoverflow.com/questions/33061926/emacs-evil-space-as-a-prefix-key-in-motion-state#33408565
    (define-key evil-motion-state-map " " nil)

    (mars-map
      ;; Remaps evil-search-forward to swiper
      [remap evil-search-forward] 'swiper)

    (mars-map
      ;; Window resizing
      "C-h" 'shrink-window-horizontally
      "C-l" 'enlarge-window-horizontally
      "C-j" 'shrink-window
      "C-k" 'enlarge-window

      ;; Custom bindings on unused evil bingins
      "!" 'universal-argument)

    (mars-map/windows
      ;; Window motion
      "h" 'evil-window-left
      "l" 'evil-window-right
      "j" 'evil-window-down
      "k" 'evil-window-up

      ;; Window manipulation
      "v" (lambda () (interactive) (evil-window-vsplit) (other-window 1))
      "s" (lambda () (interactive) (evil-window-split) (other-window 1))
      "q" 'delete-window))

  (use-package evil-collection
    :demand t
    :config
    (evil-collection-init))

  (use-package evil-magit
    :after magit
    :demand t)

  (use-package evil-exchange
    :demand t
    :general
    (mars-map
      :prefix "g"
      "x" 'evil-exchange
      "X" 'evil-exchange-cancel))

  (use-package targets
    :straight (:host github :repo "noctuid/targets.el" :branch "baf1e9f19487085c17b59f9f28e3f46b360db475")
    :demand t
    :config
    (targets-setup t)))

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
                                     (unpackaged/smerge-hydra/body)))))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode 1)

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
  :disabled
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
    :demand t
    :config
    ;; Always display the entire suggestion list onscreen, placing it
    ;; above the cursor if necessary.
    (setq company-tooltip-minimum company-tooltip-limit)
    (global-company-mode 1)
    :general
    (:keymaps 'company-active-map
     "RET" 'company-complete-selection))

  (use-package company-box
    :hook (company-mode . company-box-mode))

  (use-package company-prescient
    :demand t
    :after company
    :config (company-prescient-mode 1))

  (use-package yasnippet
    :init (yas-global-mode 1))

  (use-package yasnippet-snippets))

(use-feature text
  :init
  (use-feature abbrev))

;; Jump on things
(use-package avy
  :config (avy-setup-default)
  :general
  (mars-map
    :prefix "g"
    "J" 'avy-goto-line-below
    "K" 'avy-goto-line-above
    "w" 'avy-goto-word-0-below
    "W" 'avy-goto-word-0-above))

(use-package evil-snipe
  :demand t
  :init
  (evil-snipe-override-mode 1)
  (evil-snipe-mode 1)
  (setq evil-snipe-scope 'buffer)
  (setq evil-snipe-repeat-scope 'buffer))

(use-feature electric-mode
  :init
  (add-hook 'prog-mode-hook #'electric-pair-mode)
  (add-hook 'prog-mode-hook #'electric-indent-mode)
  (add-hook 'prog-mode-hook #'electric-layout-mode)
  (add-hook 'prog-mode-hook #'electric-quote-mode))

(use-package electric-operator
  :commands electric-operator-mode
  :demand t
  :config
  (add-hook 'prog-mode-hook #'electric-operator-mode)
  (electric-operator-add-rules-for-mode 'prog-mode
					(cons "=>" " => "))
  (electric-operator-add-rules-for-mode 'emacs-lisp-mode
					(cons "-" nil)))

(use-package hungry-delete
  :init (hungry-delete-mode))

;; Prettier code
(global-prettify-symbols-mode 1)

;; Language packages
(use-feature lisp
  :init
  (defun mars/lisp-butt-display ()
    "Function to produce nicer lisp butts.
This function can be hooked into the modes of interest.  E.g. "
    (font-lock-add-keywords
     nil
     '((")\\())+\\))"
	(1 (compose-region
	    (match-beginning 1) (match-end 1)
	    ".")
	   nil)))))

  (add-hook 'emacs-lisp-mode-hook #'mars/lisp-butt-display)

  (use-package lispy
    :hook ((emacs-lisp-mode clojure-mode cider-mode) . lispy-mode)
    :config

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
	     (t $else))))))))

(use-package yaml-mode
  :config (setq yaml-indent-offset 4))

(use-feature javascript
  :init
  (setq js-indent-level 2)

  (use-package rjsx-mode
    :init
    (defun mars/eslint-locate ()
      "Locates eslint in current directory"
      (interactive)
      (let* ((root (locate-dominating-file
		    (or (buffer-file-name) default-directory)
		    "node_modules"))
	     (eslint (and root
			  (expand-file-name "node_modules/eslint/bin/eslint.js"
					    root))))
	(when (and eslint (file-executable-p eslint))
	  (setq-local flycheck-javascript-eslint-executable eslint))))

    (mars/set-pretty-symbols 'rjsx-mode '(("() =>" . "λ")
					  ("===" . "⩶")))

    (add-hook 'rjsx-mode-hook #'mars/eslint-locate)

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
			     "--use-tabs" "false")))

  (use-package js2r-refactor
    :straight (:host github :repo "magnars/js2-refactor.el")
    :after 'rjsx-mode
    :init (add-hook 'rjsx-mode-hook #'js2-refactor-mode)))

(use-feature python
  :init
  (defun mars/python-reload ()
    (interactive)
    (when (and (eq major-mode 'python-mode)
	       python-main-file)

      (when-let* ((python-file (expand-file-name python-main-file (projectile-project-root)))
		  (exists (file-exists-p python-file))
		  (buffer-to-eval (or (find-buffer-visiting python-file)
				      (find-file python-file))))
	(save-excursion
	  (with-current-buffer buffer-to-eval
	    (when (python-shell-get-process)
	      (set-process-query-on-exit-flag (python-shell-get-process) nil)
	      (kill-process (python-shell-get-process))
	      (sleep-for 0.5))

	    (run-python nil t nil)
	    (python-shell-send-file python-file)
	    (other-window 1))))))

  (add-hook 'after-save-hook #'mars/python-reload))

(use-feature lsp
  :commands 'lsp
  :hook prog-mode
  :init
  (use-package lsp-mode
    :config
    (require 'lsp-clients)
    (add-hook 'before-save-hook
	      (lambda () (when (eq major-mode 'lsp-mode)
		      (lsp-format-buffer)))))

  (use-package lsp-ui
    :general
    (:keymaps 'lsp-ui-mode-map
     :states '(normal visual)
     "g d" 'lsp-ui-peek-find-definitions))

  (use-package company-lsp))

(use-feature clojure
  :init
  (use-package cider
    :config
    (setq clojure-indent-style :align-arguments)

    :general
    (:keymaps 'cider-repl-mode-map
     :states '(normal insert)
     "RET" 'cider-repl-return
     "<up>" 'cider-repl-backward-input
     "<down>" 'cider-repl-forward-input))

  (use-package clj-refactor
    :hook (clojure-mode-hook . clj-refactor-mode)

    :config
    ;; Automatically sort project dependencies after changing them.
    (setq cljr-auto-sort-project-dependencies t)))

;; sql
(use-package ob-sql-mode
  :demand t
  :init
  (setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "sql-mode")))))

;; UI
(use-package window-purpose
  :straight (:host github :repo "bmag/emacs-purpose")
  :demand t
  :config
  (purpose-mode)
  (require 'window-purpose-x)

  (purpose-x-popwin-setup)

  (mars/add-to-list purpose-user-mode-purposes
		    (prog-mode . code)
		    (inferior-python-mode . repl))

  (mars/add-to-list purpose-x-popwin-major-modes
		    help-mode
		    helpful-mode
		    shell-mode
		    eshell-mode)

  (mars/add-to-list purpose-special-action-sequences
		    (repl purpose-display-reuse-window-purpose))


  (setq purpose-x-popwin-width 0.3
	purpose-x-popwin-position 'right)

  (purpose-compile-user-configuration)
  (purpose-x-popwin-update-conf))

;; Font
(use-package font-size
  :straight (:host github :repo "nabeix/emacs-font-size")
  :demand t
  :init
  (setq mars-font "Ubuntu Mono"
	mars-font-height 12)
  (set-face-attribute 'default nil
		      :family mars-font)

  :config
  (font-size-init mars-font-height))

;; Frames
(mars/add-to-list default-frame-alist
		  (width . 1600)
		  (height . 900))

;e; desktop-save-mode
(use-feature desktop-save-mode
  :init
  (desktop-save-mode)
  (setq desktop-save-mode t))

(use-package framegroups
  :straight (:host github :repo "noctuid/framegroups.el")
  :demand t
  :config
  (setq fg-hide-with-xdotool nil)

  (setq mars/magit-frame-prefix "magit: "
	mars/core-frame-prefix "core: ")

  (defun mars/get-core-projectile-frame (&optional project-root)
    (interactive)
    (concat mars/core-frame-prefix (or project-root (projectile-project-root))))

  (defun mars/projectile-switch-project-action (project-root)
    (interactive)
    (fg-create-frame (mars/get-core-projectile-frame project-root)))

  (defun mars/set-core-projectile-frame (project-root)
    (interactive)
    (setq-local projectile-project-root project-root))

  (setq counsel-projectile-switch-project-action #'mars/projectile-switch-project-action)

  (defun mars/after-frame-switch-hook (frame-name &optional new)
    (interactive)
    (message frame-name)
    (when-let* ((split (s-split ": " frame-name))
		(prefix (nth 0 split))
		(project-root (nth 1 split)))
      (setq-local projectile-project-root project-root)
      (pcase prefix
	("magit"
	 (cd project-root)
	 (magit-status))

	("core"
	 (when new
	   (cd project-root)
	   (counsel-projectile-find-file))))))

  (add-hook 'fg-create-hook (lambda (frame-name) (mars/after-frame-switch-hook frame-name 't)))
  (add-hook 'fg-after-switch-hook #'mars/after-frame-switch-hook)

  (defun mars/magit-frame ()
    (interactive)
    (let ((frame-name (concat mars/magit-frame-prefix (projectile-project-root))))
      (fg-switch-to-frame frame-name)))

  :general
  (mars-map/applications
    "g" 'mars/magit-frame)

  (mars-map/frames
    "s" 'fg-switch-to-frame
    "k" 'delete-frame))

;; Use ediff on the same window
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Highlight lisp expressions
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

(setq show-paren-style 'expression
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(use-package doom-modeline
  :demand t
  :config (doom-modeline-init))

(use-package dracula-theme)

(use-package darktooth-theme
  :demand t
  :init (setq size mars-font-height
	      default-size mars-font-height)
  :config (load-theme 'darktooth 'confirm))

(use-package kaolin-themes)

(use-package solarized-theme)

(use-package material-theme)

(use-package zenburn-theme)

(use-package sublime-themes)

(use-package highlight-numbers
  :commands highlight-numbers-mode
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-quoted
  :commands highlight-quoted-mode
  :hook ((clojure-mode cider-mode emacs-lisp-mode) . highlight-quoted-mode))

(use-package highlight-defined
  :commands highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(set-face-attribute 'show-paren-match nil
		    :foreground 'unspecified
		    :background 'unspecified)

(set-face-attribute 'show-paren-match-expression nil
		    :weight 'bold
		    :slant 'italic)

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-dired
  :demand t
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :demand t
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (mars/add-to-list all-the-icons-ivy-file-commands
		    counsel-projectile-find-file
		    counsel-find-file
		    counsel-projectile
		    counsel-dired-jump)
  (mars/add-to-list all-the-icons-ivy-buffer-commands counsel-projectile-switch-to-buffer)
  (all-the-icons-ivy-setup))

(use-package centered-cursor-mode
  :demand t
  :config
  (global-centered-cursor-mode 1))

(use-package solaire-mode
  :demand t
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'magit-mode-hook #'solaire-mode))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))
  :general
  (mars-map/windows
    "w" 'ace-window))

(use-package hide-mode-line
  :init
  (add-hook 'org-mode-hook #'hide-mode-line-mode)
  (add-hook 'magit-mode-hook #'hide-mode-line-mode))

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

(use-feature shell
  :init
  (push (cons "\\*shell.*\\*" display-buffer--same-window-action) display-buffer-alist)
  (use-package shell-here)
  :general
  (mars-map/applications "s" 'shell-here))

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

(use-package olivetti
  :hook (org-mode-hook . olivetti-mode)
  :config (olivetti-set-width 120))

(use-package eboy
  :straight (:host github :repo "vreeze/eboy")
  :demand t)

;; Starts emacs server
(server-start)
