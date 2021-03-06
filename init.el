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
    "k" 'kill-current-buffer
    "b" 'counsel-switch-buffer
    "e" (lambda ()
	  (interactive)
	  (switch-to-buffer "*el scratch*")
	  (emacs-lisp-mode)))

  (mars-map/eval
    "b" 'eval-buffer
    "r" 'eval-region
    "f" 'eval-defun
    "e" 'eval-expression)

  (mars-map/files
    "f" #'counsel-find-file
    "e" (lambda () (interactive) (find-file user-init-file))
    "r" #'counsel-recentf))

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

    (:keymaps 'org-agenda-mode-map
	      :states '(normal visual emacs)
	      "RET" 'org-agenda-switch-to
	      "j" 'org-agenda-next-line
	      "k" 'org-agenda-previous-line)

    (mars-map/ivy
      "t" 'counsel-org-tag))

  ;; Prettier org
  (use-package org-bullets
    :init (add-hook 'org-mode-hook #'org-bullets-mode)))

(use-feature feature/write-mode
  :init
  (use-package writeroom-mode
    :demand t
    :config
    (setq mars/writeroom-mode-width 100)

    (defun mars/writeroom--enable ()
      "Set writeroom-width according to fill-column"
      (setq-default fill-column mars/writeroom-mode-width
		    writeroom-width mars/writeroom-mode-width
		    truncate-lines nil
		    word-wrap t)

      (auto-fill-mode)
      (writeroom-mode 1)
      (visual-line-mode 1))

    (defun mars/writeroom--disable ()
      "Set writeroom-width according to fill-column"
      (auto-fill-mode 0)
      (writeroom-mode 0)
      (visual-mode-line 0))
    
    (define-minor-mode mars/writeroom-mode
      "Minor mode for distraction-free writing."
      :init-value nil :lighter nil :global nil
      (if mars/writeroom-mode
	  (mars/writeroom--enable)
	(mars/writeroom--disable)))))

(use-package restart-emacs)

;; Edit root file
(use-package sudo-edit)

(use-package string-inflection)

;; Candidate selection

;; Select things in the minibuffer
(use-feature feature/ivy
  :init
  (use-package ivy
    :demand t
    :config
    (ivy-mode)
    (setq ivy-height 20)
    (dolist (history '(counsel-grep-history
		       counsel-git-grep-history
		       swiper-history
		       grep-regexp-history
		       ivy-history
		       counsel-M-x-history
		       counsel-describe-symbol-history))
      (defvaralias history 'regexp-search-ring))

    :general
    (mars-map
      "'" 'ivy-resume)

    (:keymaps 'ivy-minibuffer-map
	      "C-j" 'ivy-next-line
	      "C-k" 'ivy-previous-line))

  (use-package ivy-hydra)

  (use-package counsel
    :demand t
    :init
    (counsel-mode)

    :config
    (unless (string-match-p "-z --sort path" counsel-rg-base-command)
      (setq counsel-rg-base-command
            (concat counsel-rg-base-command " -z --sort path")))

    :general
    (mars-map
      "'" 'ivy-resume
      "<f5>" 'counsel-M-x
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
      "i" 'counsel-imenu))

  ;; Provide statistics for sorting/filtering
  (use-package prescient
    :config
    (prescient-persist-mode))

  (use-package ivy-prescient
    :demand t
    :config
    (require 'counsel)
    (ivy-prescient-mode))

  (use-package ivy-rich
    :after (counsel ivy)
    :demand t
    :config
    (ivy-rich-mode)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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
	(s-match (rx "(" (group (zero-or-more anything)) ")") it)
	(cadr it)
	(s-upcase it)
	(format "[%s] " it)
	(insert it)))

    ;; Enable magit-file-mode
    (add-hook 'prog-mode-hook #'magit-file-mode)

    ;; Refresh after a save.
    (mars/defhook mars/refresh-magit-after-save ()
      after-save-hook
      "Refresh magit buffers after current buffer is saved"
      (when (vc-git-responsible-p (buffer-file-name))
	(magit-refresh)))

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

  ;; Create URLs for files and commits in GitHub/Bitbucket/GitLab/... repositories
  (use-package git-link)

  (use-package git-auto-commit-mode)

  (use-package diff-hl
    :disabled
    :hook (prog-mode . diff-hl-mode)
    :config
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    (add-hook 'diff-hl-mode-hook #'diff-hl-margin-mode)
    (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)))

;; Disable built in emacs vc as we have magit for that
(use-feature feature/vc-hooks
  :config
  (setq vc-handled-backends nil))

;; Project management
(use-feature feature/projectile
  :init
  (use-package projectile
    :config (projectile-mode 1)

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

    (defun mars/projectile-close-all-with-ext (&optional arg)
      (interactive "P")
      (let* ((project-file-buffers
	      (projectile-buffers-with-file (projectile-project-buffers)))
	     (extensions (cl-remove-duplicates
			  (-non-nil
			   (--map (file-name-extension
				   (buffer-file-name it)) project-file-buffers))
			  :test #'string-equal))
	     (wanted-extension
	      (if (eq (length extensions) 1)
		  (nth 0 extensions)
		(completing-read "extension: " extensions)))
	     (buffers-matching (--filter (and
					  (or arg
					      (not (eq (current-buffer) it)))
					  (string-equal wanted-extension
							(file-name-extension
							 (buffer-file-name it))))
					 project-file-buffers)))
	(dolist (buffer buffers-matching)
	  (kill-buffer buffer))))

    (mars/add-to-list projectile-globally-ignored-directories
      "straight"
      "node_modules"
      ".metals"))

  (use-package counsel-projectile
    :straight (:host github :repo "ericdanan/counsel-projectile")
    :demand t

    :config
    (setq projectile-indexing-method 'alien)
    :general
    (mars-map
      "SPC SPC" 'counsel-projectile
      "?" 'counsel-projectile-ag)

    (mars-map/projects
      "p" 'counsel-projectile-switch-project
      "b" 'counsel-projectile-switch-to-buffer
      "k" 'projectile-kill-buffers)

    (mars-map/applications
      "s" 'projectile-run-eshell)))

;; Editor features
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
	  evil-move-cursor-back nil
	  evil-echo-state nil
	  evil-want-minibuffer nil)

    :config
    (evil-mode 1)

    ;; Unbind SPC in motion-state-map
    ;; See https://stackoverflow.com/questions/33061926/emacs-evil-space-as-a-prefix-key-in-motion-state#33408565
    (define-key evil-motion-state-map " " nil)

    (mars-map
      ;; Remaps evil-search-forward to swiper
      [remap evil-search-forward] 'counsel-grep-or-swiper
      [remap evil-search-word-forward] 'swiper-thing-at-point)

    (mars-map
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
      "h" 'windmove-left
      "l" 'windmove-right
      "j" 'windmove-down
      "k" 'windmove-up

      ;; Window manipulation
      "v" (lambda () (interactive) (evil-window-vsplit) (other-window 1))
      "s" (lambda () (interactive) (evil-window-split) (other-window 1))
      "q" 'delete-window)

    (use-package evil-collection
      :demand t
      :after evil
      :config
      (evil-collection-init (remove 'lispy evil-collection-mode-list)))

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

    (use-package evil-goggles
      :demand t
      :config (add-hook 'prog-mode-hook #'evil-goggles-mode))

    (use-package evil-args
      :demand t
      :config
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

  (use-package hungry-delete
    :demand t
    :config (global-hungry-delete-mode)))

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
	  company-minimum-prefix-length 2
	  company-tooltip-align-annotations t
	  company-minimum-prefix-length 1)

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
    :after '(company prescient)
    :config (company-prescient-mode 1))

  (use-package yasnippet
    :init (yas-global-mode 1))

  (use-package yasnippet-snippets
    :demand t))

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
      js-jsx-mode java-mode scala-mode js-mode)))

;; Prettier code
(global-prettify-symbols-mode 1)

;; Language packages
(use-feature feature/lisp
  :init
  (mars/set-pretty-symbols emacs-lisp-mode
    ("defun" . "ƒ")
    ("defmacro" . "ɱ📦"))

  (mars/counsel-M-x-initial-input emacs-lisp-mode
    "emacs-lisp ")

  (use-package lispy
    :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . lispy-mode)
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
				  mark-toggle)))))

(use-package yaml-mode
  :config (setq yaml-indent-offset 4))

(use-feature feature/rust
  :init
  (use-package toml-mode)
  
  (use-package rust-mode)
  
  ;; Add keybindings for interacting with Cargo
  (use-package cargo
    :hook (rust-mode . cargo-minor-mode))

  (use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-feature feature/javascript
  :init
  (setq js-indent-level 2
	flycheck-javascript-eslint-executable "eslint_d"
	flycheck-javascript-standard-executable "eslint_d")

  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))

  (use-package js-import
    :general
    (mars-leader-map
      :keymaps 'js-jsx-mode-map
      "i" 'js-import))

  (use-package js2r-refactor
    :commands 'js2-refactor-mode
    :straight (:host github :repo "magnars/js2-refactor.el")
    :init (add-hook 'js-mode #'js2-refactor-mode))

  (use-package prettier-js
    :commands 'prettier-js
    :init
    (setq prettier-js-command "prettier_d"
	  prettier-js-args '("--trailing-comma" "es5"
			     "--print-width" "120"
			     "--single-quote" "true"
			     "--tab-width" "2"
			     "--use-tabs" "false")
	  prettier-js-show-errors nil)

    (add-hook 'after-save-hook (lambda () (when (eq major-mode 'js-mode)
				       (prettier-js))))

    :general
    (mars-leader-map
      :keymaps 'js-mode-map
      "f" 'prettier-js)))

(use-feature feature/python
  :init
  (setq python-indent-offset 4
	flycheck-python-flake8-executable "~/.local/bin/flake8")
  (use-package elpy
    :init
    (advice-add 'python-mode :before 'elpy-enable)

    :general
    (mars-leader-map
      :keymaps 'elpy-mode-map
      "m" 'elpy-shell-send-region-or-buffer)))

(use-feature feature/clojure
  :init
  (use-package clojure-mode)

  (mars/set-pretty-symbols clojure-mode
    ("partial" . "∂")
    ("defn" . "ƒ")
    ("defmacro" . "ɱ📦"))

  (mars/counsel-M-x-initial-input emacs-lisp-mode
    "emacs-lisp ")

  (use-package lispy
    :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . lispy-mode)
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
				  mark-toggle)))))

(use-package yaml-mode
  :config (setq yaml-indent-offset 4))

(use-feature feature/rust
  :init
  (use-package toml-mode)
  
  (use-package rust-mode)
  
  ;; Add keybindings for interacting with Cargo
  (use-package cargo
    :hook (rust-mode . cargo-minor-mode))

  (use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-feature feature/javascript
  :init
  (setq js-indent-level 2
	flycheck-javascript-eslint-executable "eslint_d"
	flycheck-javascript-standard-executable "eslint_d")

  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))

  (use-package js-import
    :general
    (mars-leader-map
      :keymaps 'js-jsx-mode-map
      "i" 'js-import))

  (use-package js2r-refactor
    :commands 'js2-refactor-mode
    :straight (:host github :repo "magnars/js2-refactor.el")
    :init (add-hook 'js-mode #'js2-refactor-mode))

  (use-package prettier-js
    :commands 'prettier-js
    :init
    (setq prettier-js-command "prettier_d"
	  prettier-js-args '("--trailing-comma" "es5"
			     "--print-width" "120"
			     "--single-quote" "true"
			     "--tab-width" "2"
			     "--use-tabs" "false")
	  prettier-js-show-errors nil)

    (add-hook 'after-save-hook (lambda () (when (eq major-mode 'js-mode)
				       (prettier-js))))

    :general
    (mars-leader-map
      :keymaps 'js-mode-map
      "f" 'prettier-js)))

(use-feature feature/python
  :init
  (setq python-indent-offset 4
	flycheck-python-flake8-executable "~/.local/bin/flake8")
  (use-package elpy
    :init
    (advice-add 'python-mode :before 'elpy-enable)

    :general
    (mars-leader-map
      :keymaps 'elpy-mode-map
      "m" 'elpy-shell-send-region-or-buffer)))

(use-feature feature/clojure
  :init
  (use-package clojure-mode)

  (mars/set-pretty-symbols clojure-mode
    ("partial" . "∂")
    ("defn" . "ƒ")
    ("comp" . "●"))

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

(use-feature feature/scala
  :init
  (use-package scala-mode
    :mode "\\.s\\(cala\\|bt\\)$"
    :config
    (setq scala-indent:step 4))

  (use-package sbt-mode
    :commands sbt-start sbt-command
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows using SPACE when in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)))

(use-feature feature/lsp
  :init
  (use-package lsp-mode
    :commands 'lsp
    :init
    (mars/defhook mars/enable|lsp-mode ()
      prog-mode-hook
      "Enable lsp-mode for most programming modes."
      (unless (derived-mode-p #'emacs-lisp-mode #'clojure-mode)
	(lsp)))

    :config
    (setq lsp-prefer-flymake nil
	  lsp-response-timeout 1
	  lsp-restart 'ignore)
    (require 'lsp-clients))

  (use-package lsp-java
    :demand t
    :config
    (setq lsp-java-save-actions-organize-imports t))

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
    (mars/map
      :keymaps 'lsp-ui-mode-map
      :states '(normal visual)
      "g d" 'lsp-ui-peek-find-definitions
      [(double-mouse-1)] 'lsp-ui-peek-find-definitions
      [(down-mouse-3)] 'lsp-ui-peek-jump-backward)

    (mars-leader-map
      :keymaps 'lsp-ui-mode-map
      "f" 'lsp-ui-sideline-apply-code-actions))

  (use-package company-lsp
    :demand t
    :after company
    :config
    (push '(company-lsp :with company-yasnippet) company-backends)))

;; UI
(use-package smooth-scrolling
  :demand t
  :config
  (smooth-scrolling-mode))

(use-feature feature/display-buffer
  :init
  (setq
   side-window-rule `((display-buffer-reuse-mode-window
		       display-buffer-pop-up-window)
		      (inhibit-same-window . t)
		      (window-width . ,(/ 1.0 3.0)))

   display-buffer-alist `(("\\*.*\\*" ,@side-window-rule)
			  ("magit:.*" ,@side-window-rule))))

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
  (font-size-init mars-font-height)

  :general
  (mars-map
    "zi" 'font-size-increase
    "zo" 'font-size-decrease))

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
      (concat invocation-name "@" (system-name)))))

(setq-default 
 frame-title-format
 '(:eval (mars/get-frame-title)))

;; Use ediff on the same window

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Highlight lisp expressions

(show-paren-mode 1)
;; Highlight current line

(global-hl-line-mode 1)
(setq show-paren-style 'parenthesis
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(use-package doom-themes
  :demand t
  :init (setq size mars-font-height
	      default-size mars-font-height)
  :config
  (load-theme 'doom-one 'confirm)
  (setq window-divider-default-right-width 4
	window-divider-default-bottom-width 4)
  (setq-default mode-line-format nil)
  (doom-themes-treemacs-config)
  (fringe-mode '(6 . 6))
  
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :demand t
  :config (doom-modeline-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config (setq hl-paren-colors '("#d75f5f" "#a787af" "#87d7ff" "#0087af")))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-dired
  :demand t
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :demand t
  :config
  (mars/add-to-list all-the-icons-ivy-file-commands
    counsel-projectile-find-file
    counsel-find-file
    counsel-projectile
    counsel-dired-jump)
  (mars/add-to-list all-the-icons-ivy-buffer-commands counsel-projectile-switch-to-buffer counsel-switch-buffer)
  (all-the-icons-ivy-setup))

(use-package centered-cursor-mode
  :demand t
  :init
  (setq scroll-preserve-screen-position t
	scroll-conservatively 0
	maximum-scroll-margin 0.5
	scroll-margin 99999)
  (mars/defhook mars/enable|centered-cursor-mode ()
    prog-mode-hook
    "Enable centered-cursor-mode for most programming modes."
    (unless (derived-mode-p #'shell-mode
			    #'eshell-mode)
      (centered-cursor-mode))))

(use-package solaire-mode
  :demand t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

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

(use-package centaur-tabs
  :demand t
  :config
  (centaur-tabs-mode)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-style "wave"
	centaur-tabs-height (* 4 mars-font-height)
	centaur-tabs-set-icons t
	centaur-tabs-set-bar 'left
	centaur-tabs-set-modified-marker t)
  
  :general
  (mars-map
    "C-l" 'centaur-tabs-forward
    "C-h" 'centaur-tabs-backward)
  (mars-map/ivy
    "g" 'centaur-tabs-counsel-switch-group))

(use-feature feature/compilation
  :init
  (setq compilation-always-kill t
	compilation-scroll-output 'first-error)

  (mars/defhook mars/colorize-buffer|compilation-mode ()
    compilation-filter-hook
    "Colorize compilation buffer."
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode)))

(use-feature feature/treemacs
  :init
  (use-package treemacs
    :demand t
    :config
    (setq treemacs-tag-follow-delay (/ 1.0 3.0)
	  treemacs-file-follow-delay (/ 1.0 3.0)
	  treemacs-file-event-delay (/ 1.0 3.0)
	  treemacs-git-mode 'extended
	  treemacs-indentation 1
	  treemacs-recenter-after-file-follow t
	  treemacs-recenter-after-project-jump t)

    (dolist (face '(treemacs-root-face
                    treemacs-git-unmodified-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-ignored-face
                    treemacs-git-untracked-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face
                    treemacs-tags-face))
      (set-face-attribute face nil :family "Liberation Mono" :height 110))

    (treemacs))
  
  (use-package treemacs-projectile
    :demand t)
  
  (use-package treemacs-evil
    :demand t)
  
  (use-package treemacs-magit
    :demand t)

  (treemacs))

;; Dired
(use-feature feature/dired
  :init
  (mars-map/applications
    "d" 'dired-jump))

;; Shell
(use-feature feature/vterm
  :init
  (setq libvterm-root "~/bld/emacs-libvterm")
  (if (and module-file-suffix (file-exists-p libvterm-root))
      (progn
	(add-to-list 'load-path libvterm-root)
	(require 'vterm)

	(use-package multi-vterm
	  :straight (:host github :repo "suonlight/multi-libvterm")))
    (message "feature/vterm not supported, skipping")))

(use-feature feature/comint
  :general
  (:keymaps 'comint-mode-map
	    :states '(normal insert)
	    "C-j" nil
	    "C-k" nil
	    "<up>" 'comint-previous-input
	    "<down>" 'comint-next-input)

  :config
  (setq comint-scroll-to-bottom-on-output t))

(use-feature feature/network
  :init
  (use-package restclient))

(use-feature feature/eshell
  :config
  (setq
   ;; Send inpupt to suprocesses
   eshell-send-direct-to-subprocesses nil
   ;; Always scroll to bottom
   eshell-buffer-maximum-lines 5000))

(use-feature feature/desktop-save-mode
  :init
  (setq
   desktop-save t
   desktop-dirname ".config/emacs/var/desktop"
   desktop-load-locked-desktop t)
  
  (desktop-save-mode)

  (add-hook 'kill-emacs-hook
	    #'desktop-save-in-desktop-dir)

  (when (file-exists-p (expand-file-name desktop-base-file-name desktop-dirname))
    (desktop-read desktop-dirname))

  ;; Add a hook when emacs is closed to we reset the desktop
  ;; modification time (in this way the user does not get a warning
  ;; message about desktop modifications)
  (add-hook 'kill-emacs-hook
            (lambda ()
              ;; Reset desktop modification time so the user is not bothered
              (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))))

;; Starts emacs server
(server-start)
(put 'dired-find-alternate-file 'disabled nil)
