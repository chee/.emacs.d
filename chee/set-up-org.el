;; :PROPERTIES:
;; :ID:       1a0a4c9d-53ed-4788-831a-0c09afcbeb79
;; :END:
;; #+title: org mode setup

;; Let's just make sure this is definitely set before anything else.


;; [[file:../../notebook/docfiles/emacs/setup/org.org::+begin_src emacs-lisp :comments both :mkdirp yes :tangle ~/.emacs.d/chee/set-up-org.el][No heading:1]]
(setf org-directory *notebook-directory*)
;; No heading:1 ends here

;; Notebook


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Notebook][Notebook:1]]
(defun notebook/init nil
	(org-indent-mode t)
	(setq-local gcmh-high-cons-threshold (* 2 (default-value 'gcmh-high-cons-threshold)))
	;; let's make it feel more like a note app
	(variable-pitch-mode)
	(set-variable 'word-wrap t)
	(setf prettify-symbols-alist '())
  (prettify-symbols-mode t))
;; Notebook:1 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Notebook][Notebook:2]]
(defun notebook/configure-lob nil
  (org-babel-lob-ingest (expand-file-name "meta/library.org" org-directory)))
;; Notebook:2 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Notebook][Notebook:3]]
(defun notebook/configure-crypt nil
  (use-package org-crypt
	  :ensure nil
	  :config
	  (org-crypt-use-before-save-magic)
	  (setq org-babel-pre-tangle-hook
			'(save-buffer org-decrypt-entries))
	  (add-hook 'org-babel-post-tangle-hook #'byte-compile-current-buffer)
	  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
	  (setq org-crypt-key "yay@chee.party")
	  (setq org-crypt-disable-auto-save t)))
;; Notebook:3 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Notebook][Notebook:4]]
(defun notebook/configure-protocol nil
  ;; ensure we have a server running for protocol
  (use-package server
		:ensure nil
		:config
		(unless (server-running-p) (server-start)))
  (use-package org-protocol
		:after server
		:ensure nil))
;; Notebook:4 ends here

;; Faces

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Faces][Faces:1]]
(defun notebook/fontify-complete nil
  "Style completed headlines and checkboxes."
  ;; enable fontifying of done headlines
  (custom-set-variables '(org-fontify-done-headline t))
  ;; Style completed checkboxes like done headlines
  (font-lock-add-keywords
	  'org-mode
	  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
		    1
		    'org-headline-done prepend))
	  'append))
;; Faces:1 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Faces][Faces:2]]
(defun notebook/configure-faces nil
  "Set up org faces."
  (setf org-src-fontify-natively t)
  (notebook/fontify-complete)
  (custom-set-faces
	  '(org-default ((t (:inherit variable-pitch))))
	  '(org-document-title ((t (:height 1.60 :weight semibold))))
	  '(org-document-info-keyword ((t (:height 0.8 :slant italic :inherit fixed-pitch))))
	  '(org-list-dt ((t (:weight semibold :height 1.0))))
	  '(org-checkbox ((t (:height 1.0))))
	  '(outline-1 ((t (:height 1.2 :weight normal))))
	  '(outline-2 ((t (:height 1.18 :weight normal))))
	  '(outline-3 ((t (:height 1.16 :weight normal))))
	  '(outline-4 ((t (:height 1.14 :weight normal))))
	  '(outline-5 ((t (:height 1.12 :weight normal))))
	  '(outline-6 ((t (:height 1.1 :weight semibold))))
	  '(outline-7 ((t (:height 1.05 :weight semibold))))
	  '(outline-8 ((t (:height 1.02 :weight semibold))))
	  '(org-todo ((t (:weight semibold))))
	  '(org-done ((t (:weight semibold))))
	  '(org-headline-todo ((t (:weight regular))))
	  '(org-headline-done ((t (:weight regular))))
	  '(org-code ((t (:inherit fixed-pitch))))
	  '(org-block ((t (:inherit fixed-pitch))))
	  '(org-table ((t (:inherit fixed-pitch))))))
;; Faces:2 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Faces][Faces:3]]
(defun notebook/insert-src-block nil
  (interactive)
  (insert "#+begin_src " (read-string "Language? ") " :eval yes" "\n\n#+end_src")
  (forward-line -1))
(defun notebook/update-calendar.org nil
  (interactive)
  (async-shell-command "shortcuts run 'Update calendar.org' > /dev/null" nil nil))

(defun notebook/configure-keybindings nil
  "Setup keybindings for org-mode notebook."
  (define-prefix-command 'notebook/map)
  (bind-keys
	  ("s-n" . notebook/map)
	  ;; TODO make an notebook-mode minor mode and assign the org-mode-map
	  ;; functions there
	  :map org-mode-map
	  ("<drag-mouse-3>" . (lambda nil t))
	  ("H-<" . interupt-src-block)
	  ("C-c C-s" . interupt-src-block)
	  ("C-c C-i" . interupt-src-block)
	  ("C-c C-'" . org-edit-special)
	  :map org-src-mode-map
	  ("C-c C-'" . org-edit-src-exit)
	  ("C-c C-c" . org-edit-src-exit)
	  ;; TODO make a notebook-mode minor mode, and assign s-n there instead
	  :prefix-map notebook/map
	  :prefix "s-n"
	  ("u c" . notebook/update-calendar.org)
	  ("s" . notebook/insert-src-block)
	  ("n" . org-capture)
	  ("s-n" . org-capture)
	  ("s-a" . org-agenda)
	  ("a" . org-agenda)
	  ("d" . org-journal-new-entry)
	  ("w" . notebook/create-and-open-weeknote)
	  ("t" . org-todo-list)))
;; Faces:3 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Faces][Faces:4]]
(defun notebook/configure-agenda nil
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-start-on-weekday nil)
  (defun chee/org-agenda-acceptable-directory-p (filename)
	  (and
	    (string-match-p
		    (format "^%s[^.]" org-directory) filename)
	    (file-directory-p filename)))
  (defun chee/update-org-agenda-directories nil
	  "Rebuild the list of directories for org. It's every directory recursively."
	  (interactive)
	  (setq org-agenda-files
	    (append
		    `(,org-directory)
		    (seq-filter
		      'chee/org-agenda-acceptable-directory-p
		      (directory-files-recursively org-directory "." t)))))
  (chee/update-org-agenda-directories)
  (setq org-agenda-custom-commands


	  '(("c" "Update Calendar" agenda ""
		    ((org-agenda-mode-hook
		       (lambda nil
			       (save-window-excursion
		           (async-shell-command "shortcuts run 'Update calendar.org'")))))))))

;; TODO move to utilities, use in publish
(defun number-within-range? (number range)
  (and (>= number (car range)) (<= number (cdr range))))

(defvar emoji-ranges
  '((9984 . 10175)
	   (126976 . 127023)
	   (127136 . 127231)
	   (127248 . 127386)
	   (127462 . 129479)))

(defun emoji? (char)
  (seq-some
	  (lambda (range)
	    (number-within-range? char range))
	  emoji-ranges))

(defun emoji-name-or-identity (char)
  "Return emoji name if CHAR is emoji, otherwise return CHAR."
  (if (emoji? char)
		(get-char-code-property char 'name)
	  (char-to-string char)))

(defun replace-emojis-with-names-in-string  (string)
  "Return a string that is STRING with emojis replaced by their names."
  (mapconcat 'emoji-name-or-identity (string-to-list string) ""))

(defun slugify (headline)
  "Create a slug from a HEADLINE string."
  (replace-regexp-in-string
	  "-+" "-"
	  (replace-regexp-in-string
	    "-:[:a-z0-9]+:$" ""
	    (replace-regexp-in-string
	      "^\\(todo\\|done\\)?-" ""
	      (replace-regexp-in-string
		      "[^a-z0-9+=~@]" "-"
		      (downcase
		        (replace-emojis-with-names-in-string headline)))))))

(defun notebook/create-meeting-note nil
  (interactive)
  (let*
		((heading (org-get-heading))
		  (date (org-entry-get (point) "START"))
		  (link (org-store-link nil))
		  (filename
	      (concat "./meetings/" (slugify date) "--" (slugify heading) ".org")))

	  (progn
		  (org-entry-put (point) "MEETING-NOTES" (concat "<file:" filename ">"))
		  (find-file filename)
		  (insert "* " heading "@" date " :noexport: \n\n")
		  (org-entry-put (point) "MEETING" link)
		  (org-schedule nil date))))
;; Faces:4 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Faces][Faces:5]]
(defun notebook/configure-habits nil
  (use-package org-habit
		:ensure nil
		:custom
		(org-habit-show-habits t)
		(org-habit-completed-glyph ?✓)))
;; Faces:5 ends here



;; #+name:


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Faces][Faces:6]]
(defun notebook/configure-templates nil
  (setq org-capture-templates
	  '(("t" "Add a task"
		    entry (file+headline "inbox.org" "todo")
		    "* TODO %?\n %i\n")
	     ("c" "Add a backlinked task"
		     entry (file+headline "inbox.org" "todo")
		     "* TODO %?\n %i\n%f\n%F\n"))))
;; Faces:6 ends here

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Faces][Faces:7]]
(defun notebook/configure-babel nil
  (setq org-babel-load-languages
	  '((emacs-lisp . t)
	     (makefile . t)
	     (dot . t)
	     (css . t)
	     (sass . t)
	     (js . t)
	     (shell . t)
	     (ruby . t)
	     (lua . t)
	     (python . t)
	     (scheme . t)))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (setq org-structure-template-alist
	  '(("a" . "export ascii")
	     ("c" . "center")
	     ("C" . "comment")
	     ("e" . "example")
	     ("E" . "export")
	     ("EH" . "export html")
	     ("EL" . "export latex")
	     ("q" . "quote")
	     ("v" . "verse")
	     ("s" . "src")
	     ("st" . "src LANG :comments both :mkdirp yes :tangle FILE")))
  (setq org-confirm-babel-evaluate nil)
  (defun disable-flycheck-checkdoc-locally ()
	  (setq-local
	    flycheck-disabled-checkers
	    '(emacs-lisp-checkdoc)))
  (use-package ob-typescript :ensure t)
  (use-package ob-mermaid :ensure t)
  (use-package mermaid-mode :ensure t
	  :mode "\\.mmd\\'"
	  :config
	  (setq ob-mermaid-cli-path "mmdc"))
  (use-package ob-graphql :ensure t)

  (add-hook 'org-src-mode-hook 'disable-flycheck-checkdoc-locally)
  (add-hook 'org-src-mode-hook 'chee/reindent-buffer))
;; Faces:7 ends here

;; Creating special notes


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Creating special notes][Creating special notes:1]]
(defun notebook/create-and-open-new-note ()
  (interactive)
  (let ((name (read-string "Name for new note? ")))
	  (find-file (concat "~/notebook/notes/" name ".org"))))
;; Creating special notes:1 ends here

;; Calendar

;; file:../packages/agenda-app.org.


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Calendar][Calendar:1]]
(use-package agenda-app :ensure nil
  :after org)
;; Calendar:1 ends here

;; Main


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Main][Main:1]]
(defun notebook/configure-org nil
  (use-package org-loaddefs :ensure nil)
  (add-hook 'org-mode-hook 'notebook/init)
  (unbind-key "s-n")
  (setq diary-file "~/notebook/diary")
  (notebook/configure-keybindings)
  (notebook/configure-agenda)
  (notebook/configure-lob)
  (notebook/configure-crypt)
  (notebook/configure-faces)
  (notebook/configure-protocol)
  (notebook/configure-templates)
  (notebook/configure-babel)
  (notebook/configure-habits)
  (use-package docfiles :ensure nil
	  :custom (docfiles/deets
		          '(name "chee"
			           email "yay@chee.party"
			           zodiac "pisces"
			           signingkey "4A941CCE"))
	  (docfiles/directory "~/notebook/docfiles"))
  (use-package org-macro :ensure nil)
  (use-package org-mouse :ensure nil)
  (use-package org-attach :ensure nil))
;; Main:1 ends here

;; Screenshots


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Screenshots][Screenshots:1]]
(defun chee/org-image-paste (&optional delay)
  "Paste an image from the clipboard into an org note."
  (interactive)
  (use-package org-screenshot :ensure nil)
  (let ((path
	        (concat
	          (or (org-screenshot-image-directory) "./images/")
	          (substring (shell-command-to-string "getseq 3") 0 -1) ".png"))
	       (original-shell (getenv "SHELL")))
	  (setenv "SHELL" "sh")
	  (if (string-match-p
	        "public.png"
	        (shell-command-to-string "paste -t"))
	    (progn
	      (shell-command (format "paste public.png > %s" path))
	      (insert (format "#+CAPTION: \n[[file:%s]]" path))
	      (end-of-line 0))
		  (yank))
	  (setenv "SHELL" original-shell)))
;; Screenshots:1 ends here

;; Org setup

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Org setup][Org setup:1]]
(use-package org-contrib
  :ensure t
  :after org)
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (:map org-mode-map
	  ("s-i" . /oe/italic)
	  ("s-b" . /oe/bold)
	  ("s-u" . /oe/underscore)
	  ("s-C" . /oe/code)
	  ("C-s-v" . 'chee/org-image-paste)
	  ("M-[" . 'org-metaleft)
	  ("M-]" . 'org-metaright))
  :config
	(defun /oe/italic nil (interactive) (org-emphasize ?/))
	(defun /oe/bold nil (interactive) (org-emphasize ?*))
	(defun /oe/underscore nil (interactive) (org-emphasize ?_))
	(defun /oe/code nil (interactive) (org-emphasize ?~))
	(defun chee/auto-tangle nil
		"Auto tangle org files on save. Can't believe it took me a year to add this."
    (let ((buffer-file-name (buffer-file-name)))
		  (when (and
              buffer-file-name
              (eq major-mode 'org-mode)
              (not (string-match "_archive$" buffer-file-name)))
				  (if (string-match
								  (expand-file-name docfiles/directory)
								  (expand-file-name buffer-file-name))
						  (docfiles/tangle-file buffer-file-name)
						  (org-babel-tangle)))))
	(add-hook 'after-save-hook 'chee/auto-tangle)
	(setf org-todo-keywords
		'((sequence
				"TODO(t)"
				"ACTIVE(a!)"
				"|"
				"DONE(d!)")
			 (type "STUCK(s@)" "|" "PLANNED(p@)" "DELEGATED(>@)" "CANCELLED(c@)")))


	(setf org-use-fast-todo-selection 'expert)
	(setf org-directory *notebook-directory*)
	(setf org-html-html5-fancy t)
	(setf org-html-doctype "html5")
	(setf org-html-footnote-format "<span class=fn-number>%s</span>")
	(setf org-html-footnotes-section
		(concat
			"<aside id=footnotes>"
			"<h2 class=footnotes>%s</h2>"
			"<div id=text-footnotes>%s</div></aside>"))
	(notebook/configure-org)
	(setf htmlize-pre-style t)
  ;; 1.0 here makes it fit the actual window
  (setf org-image-actual-width '(1.0))
	(setf org-pretty-entities t)
	(setf org-use-sub-superscripts '{})
	(setf org-pretty-entities-include-sub-superscripts t)
	(setf org-startup-with-inline-images t)
	(setf org-startup-folded :nofold)
	(setf org-default-notes-file
    (expand-file-name "notes/inbox.org" org-directory))
	(setf org-startup-truncated nil)
	(setq org-src-tabs-act-natively t)
  (setf org-edit-src-content-indentation 0)
	(setf org-edit-turn-on-auto-save t)
	(setf org-edit-src-auto-save-idle-delay 5)
	(setf org-src-window-setup 'current-window)
	(setf org-fast-tag-selection-include-undo t)
	(setf org-refile-use-outline-path 'file)
	(setf org-refile-allow-creating-parent-nodes
	  'confirm)
	(setf org-refile-targets
		'((nil :maxlevel . 2)
			 (org-agenda-files :maxlevel . 3)))
	(setf org-fontify-quote-and-verse-blocks t)
	(setf org-fontify-todo-headline t)
	(setf org-agenda-buffer-tmp-name "hello")
	(setf org-mobile-directory (expand-file-name "mobile/" org-directory))
	(setf org-indent-mode-turns-on-hiding-stars nil)
	;; thanks, org-mode, for supporting my poor sleeping habits
	;; (setq org-extend-today-until 2)
	(unbind-key "M-<left>" org-mode-map)
	(unbind-key "M-<right>" org-mode-map))
(defun send-to-apple-reminders nil
	(interactive)
	(let* ((emacs-time (org-get-scheduled-time (point)))
					(reminder (substring-no-properties (org-get-heading t t t)))
					(time (format-time-string "%F %R" emacs-time)))
		(shell-command
			(format
				"reminders add emacs %s -d %s"
				(shell-quote-argument reminder)
				(shell-quote-argument time)))))
;; Org setup:1 ends here

;; Wrap region
;; I've been needing this for *ages*.

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Wrap region][Wrap region:1]]
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode)
  (wrap-region-add-wrappers
    '(("*" "*" nil org-mode)
       ("_" "_" nil org-mode)
       ("/" "/" nil org-mode)
       ("~" "~" nil org-mode)
       ("+" "+" nil org-mode))))
;; Wrap region:1 ends here

;; Visual fill column
;; this makes my notebook thinner and appear in the middle. it looks good, like a real app!


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Visual fill column][Visual fill column:1]]
(use-package visual-fill-column
	:ensure t
	:after org
  ;; disabling by default for now, because i want more screen
	;; :hook org-mode
	:init
	(setq visual-fill-column-center-text t)
	(setq visual-fill-column-width 100)
	(setq visual-fill-column-fringes-outside-margins t))
;; Visual fill column:1 ends here

;; Superstar
;; This lets me have cute bullets and title stars. also my todo items can have special icons related to their state.


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Superstar][Superstar:1]]
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-item-bullet-alist
	  '((42 . 9675) (43 . 10148) (45 . 8226)))
  (setq org-superstar-todo-bullet-alist
	  '(("TODO" . ?▢)
		   ("ACTIVE" . ?▣)
		   ("DONE" . ?✅)
		   ("STUCK" . ?◈)
		   ("CANCELLED" . ?✅)))
  (setq org-superstar-cycle-headline-bullets nil)
  (setq org-superstar-headline-bullets-list '(?✰ ?＊))
  (face-spec-set
	  'org-superstar-item
		'((t (:family "futura")))))
;; Superstar:1 ends here

;; Journal

;; This takes care of some stuff i used to configure by hand for my daily notes.
;; It carries over TODO notes from the day before, which is great and fits my lifestyle.


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Journal][Journal:1]]
;; this thing is cool! it carries over yesterday's todo notes
(use-package org-journal
  :ensure t
  :after org
  :config
  (setq org-journal-dir (expand-file-name "days/" org-directory))
  (setq org-journal-file-type 'daily)
  (setq org-journal-file-format "%F.org")
  (setq org-journal-time-format ""))
;; Journal:1 ends here

;; Roam

;; OK, I'll give it a try.


;; [[file:../../notebook/docfiles/emacs/setup/org.org::*Roam][Roam:1]]
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/cherries/documents/z/"))
  :config
  (bind-keys
    :map notebook/map
      :prefix-map notebook/roam/map
      :prefix "s-n"
      ("s-l" . org-roam-buffer-toggle)
      ("s-f" . org-roam-node-find)
      ("s-g" . org-roam-graph)
      ("s-i" . org-roam-node-insert)
      ("s-n" . org-roam-capture)
      ;; Dailies
      ("s-j" . org-roam-dailies-capture-today))
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-node-display-template
    (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package org-roam-protocol
  :after org-roam
  :ensure nil)

(defun eval-defun-without-ensure nil
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((end (search-forward ":ensure t"))
           (start (search-backward ":ensure t")))
      (delete-region start end)
      (eval-defun nil)
      (insert ":ensure t"))))
;; Roam:1 ends here

;; And, relax.

;; [[file:../../notebook/docfiles/emacs/setup/org.org::*And, relax.][And, relax.:1]]
(provide 'set-up-org)
;; And, relax.:1 ends here
