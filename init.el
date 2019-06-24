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
      use-package-compute-statistics t
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

(defmacro mars/defhook (name arglist hook docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOK is the hook to which to add the
function. DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (string-match-p "-hook$" (symbol-name hook))
    (error "Symbol `%S' is not a hook" hook))
  (unless (stringp docstring)
    (error "mars/defhook: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(format "%s\n\nThis function is for use in `%S'."
                docstring hook)
       ,@body)
     (add-hook ',hook ',name)))

(defmacro mars/defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "mars/defadvice: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (advice-add ',place ',where #',name)
     ',name))

(defmacro mars/add-to-list (list &rest args)
  "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (declare (indent defun))
  (dolist (item args)
    (add-to-list list item))
  list)

(defmacro mars/set-pretty-symbols (mode &rest symbols)
  "Set symbols SYMBOLS for mode MODE."
  (declare (indent defun))
  (let ((alist (mapcar (lambda (symbol)
			 `(,(car symbol) . ,(string-to-char (cdr symbol))))
		       symbols))
	(fun-name (intern (concat "mars/set-pretty-symbols|" (symbol-name mode)))))
    `(mars/defhook
	 ,fun-name ()
       ,(intern (concat (symbol-name mode) "-hook"))
       ,(concat "Set pretty symbols for `" (symbol-name mode) "’.")
       (setq-local prettify-symbols-alist (append prettify-symbols-alist ',alist)))))

(defmacro mars/counsel-M-x-initial-input (mode input)
  "Defines a M-x command for MODE with initial INPUT"
  (declare (indent defun))
  (let ((fun-name (intern (concat "mars/counsel-M-x|" (symbol-name mode))))
	(keymap-name (intern (concat (symbol-name mode) "-map"))))
    `(progn
       (defun ,fun-name ()
	 (interactive)
	 (let ((ivy-initial-inputs-alist '((counsel-M-x .
							,(if (stringp input)
							     input
							   (eval input))))))
	   (counsel-M-x)))
       (mars-leader-map
	 :keymaps ',keymap-name
	 "M-x" ',fun-name))))

;; Symbols for all modes
(mars/set-pretty-symbols prog-mode
  (">=" . "≥")
  ("<=" . "≤")
  ("&&" . "⋀")
  ("||" . "⋁"))

(setq sentence-end-double-space nil)

;; don't ask; follow symlinks to file under version control
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; put path before buffer name when uniquifying a buffer (instead of after)
(setq uniquify-buffer-name-style 'forward)

;; use system trash for file deletion (includes dired and backups)
(setq delete-by-moving-to-trash t)

;; quickly display current incomplete keystrokes in echo area
(setq echo-keystrokes 0.1)

;; save clipboard to kill ring before replacing
(setq save-interprogram-paste-before-kill t)

;; a lot of unix tools expect this; it's required for the crontab, for example
(setq require-final-newline t)

;; new in emacs 26; kill running processes without confirmation on Emacs exit
(setq confirm-kill-processes nil)

;; I don't use bidirectional text; improves speed for long lines (even when no
;; bidirectional text)
(setq-default bidi-display-reordering nil)

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
	  ("f" . "frames")
	  ("g" . "magit")))

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
    :states '(normal motion emacs visual))

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
(use-feature feature/org
  :init
  (use-package org
    :config
    (defun mars/open-gtd-file ()
      "Open gtd file."
      (interactive)
      (find-file org-default-notes-file))

    (defun mars/org-ret ()
      "Inserts a new heading and switches to insert state.
If point is on a src block, runs org-indent"
      (interactive)
      (if (org-in-src-block-p)
	  (org-return-indent)
	(org-insert-heading-respect-content)
	(evil-insert 1)))

    (defun mars/org-left ()
      "Does various things depending on where the point is"
      (interactive)
      (if (org-at-table-p)
	  (org-table-beginning-of-field 1))
      (org-shiftleft))

    (defun mars/org-right ()
      "Does various things depending on where the point is"
      (interactive)
      (if (org-at-table-p)
	  (org-table-end-of-field 1))
      (org-shiftright))

    (defun mars/org-change-display ()
      "Changes the :display keyword in a src block and execute it"
      (interactive)
      (let ((display-list '("plain" "org")))
	(save-excursion
	  (org-babel-goto-src-block-head)
	  (if (and (re-search-forward ":display" (line-end-position) t)
		   (re-search-forward (regexp-opt display-list)))
	      (when-let*
		  ((current-display (thing-at-point 'word 'no-properties))
		   (pos (member current-display display-list))
		   (next (or (cadr pos)
			     (car display-list))))
		(replace-match next)
		(org-babel-execute-src-block))
	    (end-of-line)
	    (insert " :display plain")))))

    (defun mars/org-babel-execute ()
      "Execute next executable thing."
      (interactive)
      (org-babel-next-src-block)
      (org-babel-execute-src-block))

    (defun mars/org-leader-d ()
      "Does various things, starting with a D"
      (interactive)
      (cond ((org-in-src-block-p) (mars/org-change-display))
	    (t (org-todo "DONE"))))

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
     org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "CANCELLED"))
     org-confirm-babel-evaluate nil
     org-src-window-setup 'current-window)

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
      "d" 'mars/org-leader-d
      "e" 'mars/org-babel-execute
      "c" (lambda () (interactive) (org-todo "CANCELLED")))

    (:keymaps 'org-mode-map
     :states '(normal visual)
     "<up>" 'org-metaup
     "<down>" 'org-metadown
     "<left>" 'mars/org-left
     "<right>" 'mars/org-right

     "C-j" nil
     "C-k" nil

     "?" 'counsel-org-goto

     [remap evil-shift-left] 'org-metaleft
     [remap evil-shift-right] 'org-metaright)

    (:keymaps 'org-mode-map
     :states '(normal visual insert)
     "RET" 'mars/org-ret)

    (:keymaps 'org-agenda-mode-map
     :states '(normal visual emacs)
     "RET" 'org-agenda-switch-to
     "j" 'org-agenda-next-line
     "k" 'org-agenda-previous-line)

    (mars-map/ivy
      "t" 'counsel-org-tag))

  (use-package org-trello)

  ;; Prettier org
  (use-package org-bullets
    :init (add-hook 'org-mode-hook #'org-bullets-mode)))

(use-package restart-emacs)

;; Edit root file
(use-package sudo-edit)

;; Candidate selection

;; Select things in the minibuffer
(use-feature feature/ivy
  :init
  (use-package ivy
    :config
    (ivy-mode 1)
    (setq ivy-height 20)

    :general
    (:keymaps 'ivy-minibuffer-map
     "$" 'ivy-toggle-calling
     "^" 'ivy-occur
     "#" 'ivy-posframe-read-action
     "%" 'ivy-posframe-avy
     "C-<up>" 'ivy-previous-history-element
     "C-<down>" 'ivy-next-history-element)

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
      "s" 'counsel-esh-history
      "i" 'counsel-imenu)

    ;; Merge these histories into one
    (dolist (history '(counsel-grep-history
		       counsel-git-grep-history
		       swiper-history
		       grep-regexp-history
		       ivy-history
		       counsel-M-x-history
		       counsel-describe-symbol-history))
      (defvaralias history 'regexp-search-ring)))

  (use-package ivy-hydra)

  ;; Provide statistics for sorting/filtering
  (use-package prescient
    :config (prescient-persist-mode 1))

  (use-package ivy-prescient
    :demand t
    :config (ivy-prescient-mode 1))

  (use-package ivy-rich
    :after (ivy counsel)
    :demand t
    :init
    (setq ivy-rich-path-style 'abbrev
	  ivy-virtual-abbreviate 'full
	  ivy-format-function #'ivy-format-function-line)

    :config
    (setq ivy-format-function #'ivy-format-function-line)

    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
   	  (get-buffer candidate)
	(let ((icon (all-the-icons-icon-for-mode major-mode)))
	  (if (symbolp icon)
	      (all-the-icons-icon-for-mode 'fundamental-mode)
	    icon))))

    (setq ivy-rich-display-transformers-list
	  '(ivy-switch-buffer
	    (:columns
	     ((ivy-rich-switch-buffer-icon :width 2)
	      (ivy-rich-candidate
	       (:width 30))
	      (ivy-rich-switch-buffer-size
	       (:width 7))
	      (ivy-rich-switch-buffer-indicators
	       (:width 4 :face error :align right))
	      (ivy-rich-switch-buffer-major-mode
	       (:width 12 :face warning))
	      (ivy-rich-switch-buffer-project
	       (:width 15 :face success))
	      (ivy-rich-switch-buffer-path
	       (:width
		(lambda
		  (x)
		  (ivy-rich-switch-buffer-shorten-path x
						       (ivy-rich-minibuffer-width 0.3))))))
	     :predicate
	     (lambda (cand)
	       (get-buffer cand)))
	    counsel-M-x
	    (:columns
	     ((counsel-M-x-transformer
	       (:width 40))
	      (ivy-rich-counsel-function-docstring
	       (:face font-lock-doc-face))))
	    counsel-describe-function
	    (:columns
	     ((counsel-describe-function-transformer
	       (:width 40))
	      (ivy-rich-counsel-function-docstring
	       (:face font-lock-doc-face))))
	    counsel-describe-variable
	    (:columns
	     ((counsel-describe-variable-transformer
	       (:width 40))
	      (ivy-rich-counsel-variable-docstring
	       (:face font-lock-doc-face))))
	    counsel-recentf
	    (:columns
	     ((ivy-rich-candidate
	       (:width 0.8))
	      (ivy-rich-file-last-modified-time
	       (:face font-lock-comment-face))))))

    (ivy-rich-mode))


  (use-package wgrep)

  (use-package ivy-yasnippet
    :general
    (mars-map/ivy
      "a" 'ivy-yasnippet)))

(use-package transient
  :after magit
  :general
  (:keymaps 'transient-map
   "<escape>" 'transient-quit-one))

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
(use-feature feature/git
  :init
  (use-package magit
    :config
    (global-magit-file-mode 1)
    ;; Auto commits in wip refs
    (magit-wip-mode 1)
    (setq
     magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
     ;; Always save everything before opening a magit buffer
     magit-save-repository-buffers 'dontask
     ;; Cursor on Unstaged Changes by default
     magit-status-initial-section '(1 2)
     magit-section-initial-visibility-alist '((stashes . hide)
					      (untracked . hide))
     magit-log-section-commit-count 20
     ;; Don’t ask when doing stuff
     magit-commit-ask-to-stage nil
     magit-no-confirm
     '(reverse
       rename
       resurrect
       untrack
       trash
       delete
       abort-rebase
       abort-merge
       merge-dirty
       drop-stashes
       reset-bisect
       kill-process
       delete-unmerged-branch
       delete-pr-branch
       remove-modules
       stage-all-changes
       const unstage-all-changes
       safe-with-wip)
     magit-status-show-hashes-in-headers t
     magit-list-refs-sortby "-creatordate")

    ;; Auto revert buffers
    (magit-auto-revert-mode)
    (global-auto-revert-mode)
    (setq auto-revert-interval 1
	  magit-auto-revert-immediately t
	  revert-without-query '(".*"))

    (mars/defhook mars/git-commit-setup ()
      git-commit-mode-hook
      "Insert diminished current branch name between [], at the start of the commit message"
      (-some--> (vc-git-branches)
		(car it)
		(s-split-words it)
		(when (>= (length it) 2)
		  (-take 2 it))
		(mapcar #'s-upcase it)
		(apply 'format "[%s-%s] " it)
		(insert it)))

    ;; Enable magit-file-mode
    (add-hook 'prog-mode-hook #'magit-file-mode)

    ;; Refresh after a save.
    (add-hook 'after-save-hook #'magit-refresh)

    :general
    (mars-map/applications
      "g" 'magit-status)

    (mars/map
      :prefix "-"
      "m" 'magit-merge
      "-" 'magit-dispatch
      "s" 'magit-stage
      "u" 'magit-unstage
      "c" 'magit-commit
      "p" 'magit-push
      "f" 'magit-fetch
      "b" 'magit-branch-or-checkout
      "F" 'magit-pull
      "r" 'magit-rebase
      "O" 'magit-reset
      "L" 'magit-log
      "l" 'magit-log-current
      "z" 'magit-stash
      "u" 'magit-unstage
      "x" 'magit-discard
      "d" 'magit-diff-buffer-file))

  ;; git forges
  (use-package forge
    :disabled
    :after magit)

  ;; Create URLs for files and commits in GitHub/Bitbucket/GitLab/... repositories
  (use-package git-link)

  (use-package git-auto-commit-mode)

  (use-package diff-hl
    :hook (prog-mode . diff-hl-mode)
    :config
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; Disable built in emacs vc as we have magit for that
(use-feature feature/vc-hooks
  :config
  (setq vc-handled-backends nil))

;; Project management
(use-feature feature/projectile
  :init
  (use-package projectile :config (projectile-mode 1))

  (use-package counsel-projectile
    :straight (:host github :repo "ericdanan/counsel-projectile")
    :demand t

    :config
    (mars/add-to-list projectile-globally-ignored-directories
      "straight"
      "node_modules")

    (mars/defhook mars/colorize-buffer|compilation-mode ()
      compilation-filter-hook
      "Colorize compilation buffer."
      (toggle-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point))
      (toggle-read-only))

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
      "b" 'counsel-projectile-switch-to-buffer
      "k" 'projectile-kill-buffers)

    (mars-map/applications
      "s" 'projectile-run-eshell)))

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
 ;; Don't ask for confirmation for .dir-locals
 enable-local-variables :all)

;; Vim-like keybindings.
(use-feature feature/evil
  :init
  (use-package evil
    :demand t
    :init
    ;; Required for ollection
    (setq evil-want-integration t
	  evil-want-keybinding nil
	  evil-symbol-word-search t
	  evil-move-cursor-back nil)

    :config
    (evil-mode 1)

    ;; Unbind SPC in motion-state-map
    ;; See https://stackoverflow.com/questions/33061926/emacs-evil-space-as-a-prefix-key-in-motion-state#33408565
    (define-key evil-motion-state-map " " nil)

    (mars-map
      ;; Remaps evil-search-forward to swiper
      [remap evil-search-forward] 'counsel-grep-or-swiper)

    (mars-map
      ;; Window resizing
      "C-h" 'shrink-window-horizontally
      "C-l" 'enlarge-window-horizontally
      "C-j" 'shrink-window
      "C-k" 'enlarge-window

      ;; Visual
      "_" 'evil-visual-block

      ;; Custom bindings on unused evil bingins
      "!" 'universal-argument

      ;; Replaces j,k by non-blank versions
      "j" 'evil-next-line
      "k" 'evil-previous-line)

    (define-key universal-argument-map "!" 'universal-argument-more)

    (mars-map/windows
      ;; Window motion
      "h" 'evil-window-left
      "l" 'evil-window-right
      "j" 'evil-window-down
      "k" 'evil-window-up

      ;; Window manipulation
      "v" (lambda () (interactive) (evil-window-vsplit) (other-window 1))
      "s" (lambda () (interactive) (evil-window-split) (other-window 1))
      "q" 'delete-window)

    (use-package evil-collection
      :demand t
      :config
      (evil-collection-init))

    (use-package evil-magit
      :after magit
      :demand t
      :config
      (setq evil-magit-use-z-for-folds t))

    (use-package evil-exchange
      :demand t
      :general
      (mars-map
	:prefix "g"
	"x" 'evil-exchange
	"X" 'evil-exchange-cancel))

    ;; Needed for js2-refactor
    (use-package multiple-cursors)

    (use-package evil-mc
      :straight (:host github :repo "gabesoft/evil-mc")
      :demand t
      :config (global-evil-mc-mode)

      :general
      (mars-map
	:states 'visual
	"I" 'evil-mc-make-cursor-in-visual-selection-beg
	"A" 'evil-mc-make-cursor-in-visual-selection-end))

    (use-package evil-goggles
      :demand t
      :config (add-hook 'prog-mode-hook #'evil-goggles-mode))

    (use-package evil-args
      :demand t
      :config
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))))

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

(use-feature feature/save-place
  :init
  (save-place-mode 1))

;; Save things when switching buffers and other things
(use-package super-save
  :demand t
  :config
  (super-save-mode 1))

(use-feature feature/subword
  :init
  (global-subword-mode 1))

;; Surround things.
(use-package evil-surround
  :demand t
  :config (global-evil-surround-mode 1))

(use-package expand-region
  :general
  (:keymaps 'evil-visual-state-map
   "v" 'er/expand-region
   "V" 'er/contract-region))

(use-package multi-line
  :demand t)

;; Completion
(use-feature feature/completion
  :init
  (use-package company
    :demand t
    :config
    ;; Always display the entire suggestion list onscreen, placing it
    ;; above the cursor if necessary.
    (setq company-tooltip-minimum company-tooltip-limit
	  company-idle-delay 0.2
	  company-tooltip-limit 15
	  company-minimum-prefix-length 2)

    (global-company-mode 1)

    (defun mars/company-backend-with-yas (backends)
      "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
      (if (and (listp backends) (memq 'company-yasnippet backends))
	  backends
	(append (if (consp backends)
		    backends
		  (list backends))
		'(:with company-yasnippet))))

    ;; add yasnippet to all backends
    (setq company-backends
          (mapcar #'mars/company-backend-with-yas company-backends))

    ;; Use helpful in company elisp help buffers
    (mars/defadvice mars/elips-use-helpful|company
	(func &rest args)
      :around elisp--company-doc-buffer
      "Cause `company' to use Helpful to show Elisp documentation."
      (cl-letf (((symbol-function #'describe-function) #'helpful-function)
		((symbol-function #'describe-variable) #'helpful-variable)
		((symbol-function #'help-buffer) #'current-buffer))
	(apply func args)))

    :general
    (mars/map
      :states 'insert
      "<tab>" 'company-complete)

    (:keymaps 'company-active-map
     "RET" 'company-complete-selection
     "<tab>" 'complete-symbol))

  (use-package company-prescient
    :demand t
    :after company
    :config (company-prescient-mode 1))

  (use-package yasnippet
    :init (yas-global-mode 1))

  (use-package yasnippet-snippets
    :demand t
    :straight (:host github :repo "qleguennec/yasnippet-snippets"
	       :files ("*"))))

(use-feature feature/text
  :init
  (use-feature feature/abbrev))

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
  (setq evil-snipe-repeat-scope 'buffer)

  :general
  (mars/map
    ;; Jump on things
    "(" (lambda nil (interactive)
  	  (evil-snipe-F nil "("))
    ")" (lambda nil (interactive)
    	  (evil-snipe-f nil "("))
    "{" (lambda nil (interactive)
  	  (evil-snipe-F nil "{"))
    "}" (lambda nil (interactive)
  	  (evil-snipe-f nil "{"))
    "[" (lambda nil (interactive)
  	  (evil-snipe-F nil "["))
    "]" (lambda nil (interactive)
  	  (evil-snipe-f nil "["))))

(use-feature feature/electric-mode
  :init
  (add-hook 'prog-mode-hook #'electric-pair-mode)
  (add-hook 'prog-mode-hook #'electric-indent-mode)
  (add-hook 'prog-mode-hook #'electric-layout-mode)
  (add-hook 'prog-mode-hook #'electric-quote-mode)

  (use-package aggressive-indent
    :demand t
    :config
    (global-aggressive-indent-mode)
    (mars/add-to-list aggressive-indent-excluded-modes
      rjsx-mode java-mode))

  (use-package electric-operator
    :commands electric-operator-mode
    :demand t
    :config
    (add-hook 'prog-mode-hook #'electric-operator-mode)
    (electric-operator-add-rules-for-mode 'prog-mode
					  (cons ">=" " >= ")
					  (cons "<=" " <= ")
					  (cons "=" " = ")
					  (cons "=>" " => "))
    (electric-operator-add-rules-for-mode 'emacs-lisp-mode
					  (cons "-" nil))
    (electric-operator-add-rules-for-mode 'rjsx-mode
					  (cons "," " , ")
					  (cons ">=" " >= ")
					  (cons "<=" " <= ")
					  (cons "=>" " => ")
					  (cons "=" " = ")
					  (cons "==" " === ")
					  (cons "!=" " !== "))))

(use-package hungry-delete
  :demand t
  :config (global-hungry-delete-mode)
  :general
  (general-define-key "<backspace>" 'hungry-delete-backward))

;; Prettier code
(global-prettify-symbols-mode 1)

;; Language packages
(use-feature feature/lisp
  :init
  (mars/set-pretty-symbols emacs-lisp-mode
    ("defun" . "ƒ")
    ("defmacro" . "ɱ📦")
    ;; Waiting for https://github.com/ekaschalk/notate release
    ;; ("setq" . "⟾")
    )

  (mars/counsel-M-x-initial-input emacs-lisp-mode
    "emacs-lisp ")

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
				  escape
				  additional-movement
				  commentary
				  mark-toggle)))

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

(use-feature feature/javascript
  :init
  (setq js-indent-level 2)
  (use-package rjsx-mode
    :init
    (mars/set-pretty-symbols rjsx-mode
      ("===" . "⩶")
      ("import" . "⟼")
      ("export" . "⟻"))

    (mars/defhook mars/eslint-locate|rjsx-mode ()
      rjsx-mode-hook
      "Locate eslint in current directory."
      (interactive)
      (let* ((root (locate-dominating-file
		    (or (buffer-file-name) default-directory)
		    "node_modules"))
	     (eslint (and root
			  (expand-file-name "node_modules/eslint/bin/eslint.js"
					    root))))
	(when (and eslint (file-executable-p eslint))
	  (setq-local flycheck-javascript-eslint-executable eslint))))

    :config
    (mars/add-to-list auto-mode-alist
      ("\\.jsx\\'" . rjsx-mode)
      ("\\.js\\'" . rjsx-mode)
      ("\\.json\\'" . rjsx-mode))

    (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

  (use-package js-import
    :general
    (mars-leader-map
      :keymaps 'rjsx-mode-map
      "i" 'js-import))

  (use-package js2r-refactor
    :after rjsx-mode
    :commands 'js2-refactor-mode
    :straight (:host github :repo "magnars/js2-refactor.el")
    :init (add-hook 'rjsx-mode-hook #'js2-refactor-mode))

  (use-package eslintd-fix
    :commands 'eslintd-fix
    :after rjsx-mode)

  (use-package prettier-js
    :commands 'prettier-js
    :after rjsx-mode
    :init
    (setq prettier-js-command "prettier_d"
	  prettier-js-args '("--trailing-comma" "es5"
			     "--print-width" "120"
			     "--single-quote" "true"
			     "--tab-width" "2"
			     "--use-tabs" "false")
	  prettier-js-show-errors nil))

  (defun mars/reformat|rjsx-mode ()
    "Reformat javascript on save. Runs prettier + eslint"
    (interactive)
    (when (eq major-mode 'rjsx-mode)
      ;; TODO better solution
      (prettier-js)
      (eslintd-fix)))

  (add-hook 'before-save-hook #'mars/reformat|rjsx-mode)

  (mars/counsel-M-x-initial-input rjsx-mode
    (concat "^" (regexp-opt '("rjsx" "js2" "lsp")) " "))

  ;; (mars/defhook mars/fold-imports|rjsx-mode ()
  ;;   rjsx-mode-hook
  ;;   "When origami-mode is enabled, fold all import statements."
  ;;   (when origami-mode
  ;;     (save-excursion
  ;; 	(beginning-of-buffer)
  ;; 	(while (re-search-forward "^import" nil t)
  ;; 	  (call-interactively #'origami-close-node)))))
  )

(use-feature feature/python
  :init
  (use-package elpy
    :init
    (advice-add 'python-mode :before 'elpy-enable)))

(use-feature feature/debug
  :init
  (use-package dap-mode
    :hook (java-mode . dap-mode)
    :config
    (add-hook 'dap-mode-hook #'dap-ui-mode)))

(use-feature feature/lsp
  :init
  (use-package lsp-mode
    :commands 'lsp
    :init
    (mars/defhook mars/enable|lsp-mode ()
      prog-mode-hook
      "Enable lsp-mode for most programming modes."
      (unless (derived-mode-p #'emacs-lisp-mode
			      #'java-mode)
	(lsp)))

    :config
    (setq lsp-prefer-flymake nil
	  lsp-response-timeout 1)
    (require 'lsp-clients))

  (use-package lsp-java)

  (use-package lsp-ui
    :config
    (mars/defadvice mars/advice-apply-single-fix|lsp-ui (orig-fun &rest args)
      :around lsp-ui-sideline-apply-code-actions
      "Apply code fix immediately if only one is possible."
      (cl-letf* ((orig-completing-read (symbol-function #'completing-read))
		 ((symbol-function #'completing-read)
		  (lambda (prompt collection &rest args)
		    (if (= (safe-length collection) 1)
			(car collection)
		      (apply orig-completing-read prompt collection args)))))
	(apply orig-fun args)))

    :general
    (:keymaps 'lsp-ui-mode-map
     :states '(normal visual)
     "g d" 'lsp-ui-peek-find-definitions)

    (mars-leader-map
      :keymaps 'lsp-ui-mode-map
      "f" 'lsp-ui-sideline-apply-code-actions))

  (use-package company-lsp
    :after company
    :config
    (push '(company-lsp :with company-yasnippet) company-backends)))

(use-feature feature/clojure
  :disabled
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

;; UI
(use-package window-purpose
  :straight (:host github :repo "bmag/emacs-purpose")
  :disabled
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

(use-package adaptive-wrap
  :demand t
  :config (adaptive-wrap-prefix-mode))

(use-package perfect-margin
  :disabled
  :demand t
  :config
  (defun mars/activate-single-window-mode ()
    (when (eq 1 (count-windows)))
    (perfect-margin-mode))

  (define-minor-mode mars/single-window-mode
    "When activated, activate perfect-margin-mode."
    nil nil nil
    (if mars/single-window-mode
	(add-hook 'window-configuration-change-hook
		  #'mars/activate-single-window-mode)
      (remove-hook 'window-configuration-change-hook
		   #'mars/activate-single-window-mode)
      (perfect-margin-mode nil)))

  (mars/single-window-mode))

;; Font
(use-package font-size
  :straight (:host github :repo "nabeix/emacs-font-size")
  :demand t
  :init
  (setq mars-font "Overpass Mono"
	mars-font-height 10)
  (set-face-attribute 'default nil
		      :family mars-font)
  :config
  (font-size-init mars-font-height))

;; Symbols
(use-package notate
  :disabled
  :demand t
  :straight (:host github :repo "ekaschalk/notate"))

;; Fold parts of code
(use-package origami
  :commands 'origami-mode
  :init (add-hook 'prog-mode-hook #'origami-mode))

;; Frames
(mars/add-to-list default-frame-alist
  (width . 1600)
  (height . 900))

;; Frame title
(defun mars/get-frame-title ()
  "If inside a git repo return a string containing git repo, branch and file state else
return default frame title"
  (let* ((file (buffer-file-name))
         (git-directory (when file
                          (locate-dominating-file (file-truename default-directory) ".git"))))
    (if git-directory
        (concat (projectile-project-name)
		" / "
		(file-name-nondirectory (buffer-file-name))
                " [" (car (vc-git-branches)) "] " )
      (concat invocation-name "@" system-name))))

(setq-default
 frame-title-format
 '(:eval (mars/get-frame-title)))

;; desktop-save-mode
(use-feature feature/desktop-save-mode
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
    (concat mars/core-frame-prefix
	    (funcall projectile-project-name-function
		     (or project-root (projectile-project-root)))))

  (defun mars/projectile-switch-project-action (project-root)
    (interactive)
    (fg-create-frame (mars/get-core-projectile-frame (funcall projectile-project-name-function
							      project-root))))

  ;; (setq counsel-projectile-switch-project-action #'mars/projectile-switch-project-action)

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

  ;; (add-hook 'fg-create-hook (lambda (frame-name) (mars/after-frame-switch-hook frame-name 't)))
  ;; (add-hook 'fg-after-switch-hook #'mars/after-frame-switch-hook)

  ;; (defun mars/magit-frame ()
  ;;   (interactive)
  ;;   (let ((frame-name (concat mars/magit-frame-prefix (projectile-project-root))))
  ;;     (fg-switch-to-frame frame-name)))

  :general
  ;; (mars-map/applications
  ;;   "g" 'mars/magit-frame)

  (mars-map/frames
    "s" 'fg-switch-to-frame
    "f" 'fg-switch-to-last-frame
    "k" 'delete-frame))

;; Use ediff on the same window
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Highlight lisp expressions
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

(setq show-paren-style 'parenthesis
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(use-package dracula-theme)

(use-package darktooth-theme)

(use-package solarized-theme)

(use-package doom-themes)

(use-package material-theme)

(use-package zenburn-theme)

(use-package creamsody-theme
  :demand t
  :init (setq size mars-font-height
	      default-size mars-font-height)
  :config
  (load-theme 'creamsody 'confirm)
  (setq window-divider-default-right-width 2
	window-divider-default-bottom-width 2)
  (setq-default mode-line-format nil)
  (fringe-mode '(4 . 4)))

(use-package doom-modeline
  :demand t
  :config (doom-modeline-mode))

(use-package nimbus-theme)

(use-package dimmer
  :init (dimmer-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config (setq hl-paren-colors '("#d75f5f" "#a787af" "#87d7ff" "#0087af")))

(use-package highlight-numbers
  :commands highlight-numbers-mode
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-quoted
  :commands highlight-quoted-mode
  :hook ((clojure-mode cider-mode emacs-lisp-mode) . highlight-quoted-mode))

(use-package highlight-stages
  :hook ((clojure-mode cider-mode emacs-lisp-mode) . highlight-quoted-mode)
  :config
  (setq highlight-stages-highlight-real-quote nil))

(use-package highlight-defined
  :commands highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-dired
  :demand t
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :disabled
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
  :init
  (mars/defhook mars/enable|centered-cursor-mode ()
    prog-mode-hook
    "Enable centered-cursor-mode for most programming modes."
    (unless (derived-mode-p #'shell-mode
			    #'eshell-mode)
      (centered-cursor-mode))))

(use-package solaire-mode
  :demand t
  :config
  (defun mars/alternate-solaire-mode (&rest args)
    (interactive)
    (let ((solaire-mode-toggle nil)
	  (windows (winner-sorted-window-list)))
      (dolist (win windows)
	(with-current-buffer (window-buffer win)
	  (solaire-mode (if solaire-mode-toggle 1 -1)))
	(setq solaire-mode-toggle (not solaire-mode-toggle)))))
  (add-hook 'window-configuration-change-hook #'mars/alternate-solaire-mode))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)
	aw-dispatch-always t)
  :general
  (mars-map/windows
    "w" 'ace-window))

(use-feature feature/winner-mode
  :init
  (winner-mode))

(use-package transpose-frame)

(use-package rotate)

(use-package zoom
  :demand t
  :config
  (setq zoom-size '(0.66 . 0.618))
  (zoom-mode))

(use-feature feature/compilation
  :init
  (setq compilation-always-kill t
	compilation-scroll-output 'first-error)

  (mars/defadvice mars/advice-projectile|counsel-compile (orig-fun &rest args)
    :around counsel-compile
    "Set projectile root when possible."
    (if (not (projectile-project-p))
	(apply orig-fun args)
      (cd (projectile-project-root))
      (apply orig-fun (projectile-project-root) args)))

  (mars/defadvice mars/advice-remove-history|counsel-compile (&rest args)
    :after counsel-compile
    "Remove counsel-compile history after selecting a compilation string"
    (setq counsel-compile-history nil))

  (mars/defhook mars/set-buffer-name|compilation-mode (process)
    compilation-start-hook
    "Set the name of the compilation buffer according to the command being ran."
    (let ((buffer-name (concat "*compilation "
			       (string-join (process-command process) " ")
			       "*")))
      (when-let ((old-buffer (get-buffer buffer-name)))
	(kill-process (get-buffer-process old-buffer))
	(kill-buffer old-buffer))
      (rename-buffer buffer-name))))

;; Dired
(use-feature feature/dired
  :init
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff) ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)	;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)	;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay) ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))

  (use-package dired-git-info
    :commands dired-git-info-mode)

  :general
  (mars-map
    :keymaps 'dired-mode-map
    "." 'hydra-dired/body)

  (mars-map/applications
    "d" 'dired))

;; Browsers
(use-package atomic-chrome
  :demand t
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-default-major-mode 'markdown-mode))

;; Shell
(use-feature feature/comint
  :general
  (:keymaps 'comint-mode-map
   :states '(normal insert)
   "C-j" nil
   "C-k" nil
   "<up>" 'comint-previous-input
   "<down>" 'comint-next-input))

(use-feature feature/eshell
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

  ;; From https://github.com/jorgenschaefer/comint-scroll-to-bottom
  (defun comint-add-scroll-to-bottom ()
    "Activate `comint-scroll-to-bottom'.
This should be put in `comint-mode-hook' or any derived mode."
    (add-hook 'window-scroll-functions 'comint-scroll-to-bottom nil t))

  (defun comint-scroll-to-bottom (window display-start)
    "Recenter WINDOW so that point is on the last line.
This is added to `window-scroll-functions' by
`comint-add-scroll-to-bottom'.
The code is shamelessly taken (but adapted) from ERC."
    (let ((proc (get-buffer-process (current-buffer))))
      (when (and proc
		 window
		 (window-live-p window))
	(let ((resize-mini-windows nil))
          (save-selected-window
            (select-window window)
            (save-restriction
              (widen)
              (when (>= (point) (process-mark proc))
		(save-excursion
                  (goto-char (point-max))
                  (recenter -1)
		  (sit-for 0)))))))))

  (add-hook 'comint-mode-hook #'comint-scroll-to-bottom)

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

;; Starts emacs server
(server-start)
