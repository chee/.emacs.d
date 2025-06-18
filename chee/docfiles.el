;; defcustom

;; [[file:../../notebook/docfiles/emacs/packages/docfiles.org::*defcustom][defcustom:1]]
(defgroup docfiles nil "A system for configuring systems with `org'."
	 :group 'applications)

(defcustom docfiles/directory (expand-file-name "~/notebook/docfiles")
	 "The root directory for your docfiles."
	 :type 'file
	 :group 'docfiles)
;; defcustom:1 ends here

;; publish
;; The code is exported by this org publishing function.

;; Before exporting, we decrypt our entries and allow babel blocks to be evaluated without question.

;; When the special tag ~:docfileseval:~ is found in an entry's header, the first src block in it is executed before the file is tangled.

;; Headings that have a ~:system:~ tag must also have a tag that is the same as their `system-type`, such as ~:darwin:~ or ~:gnukfreebsd:~


;; [[file:../../notebook/docfiles/emacs/packages/docfiles.org::*publish][publish:1]]
;; docfiles --- Summary
;; Commentary:
;; a documented system system.
;; Code:
;; publish:1 ends here

;; check tags for system

;; If you set a header-tag of ~:system:~, you should also have a header-tag that matches a condensed version of your ~system-type~. Condensed here means ~-~ and ~/~ are removed and replaced with the empty string.

;; e.g. ~ms-dos~ becomes ~msdos~.


;; [[file:../../notebook/docfiles/emacs/packages/docfiles.org::*check tags for system][check tags for system:1]]
(defun docfiles/--tags-valid-for-system (&optional tags)
	 "Check if TAGS is valid for current system.

Either the `:system:' tag is not present, or the `:system:' tag and a
tag matching the system-type is.

the system tag is the system-type special chars `/' and `-'
removed (i.e. `:gnulinux:')."
	 (let ((system-type-tag
		(replace-regexp-in-string "[/-]" "" (symbol-name system-type))))
		(or
		 (not (member "system" tags))
		 (and (member "system" tags) (member system-type-tag tags))
		 (not tags))))
;; check tags for system:1 ends here

;; tangle-file and publish

;; here we go! we make a temporary file with the contents of the target file, cut out any subtrees that are no good for us, and execute the first code block under any headings that are marked ~:docfileseval~.


;; [[file:../../notebook/docfiles/emacs/packages/docfiles.org::*tangle-file and publish][tangle-file and publish:1]]
(defun docfiles/tangle-file (&optional file-name)
  "Extract the bodies of source code blocks in FILE-NAME.

      Skip blocks that are not available on this system with tags like
      blocks that are not available on this system with tags like
      :system:darwin:.

      Executes blocks with tag `:docfileseval:'.

      Source code blocks are extracted with `org-babel-tangle'."
  (interactive)
  (catch 'success
    (atomic-change-group
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (not (equal (point) (point-max)))
         (org-next-visible-heading 1)
         (let ((tags (org-get-tags)))
           (if (docfiles/--tags-valid-for-system tags)
               (when (member "docfileseval" tags)
                 (condition-case nil
                     (progn
                       (org-babel-next-src-block)
                       (org-babel-execute-src-block-maybe))
                   (error nil)))
             (org-cut-subtree))))
         (condition-case nil (funcall-interactively 'org-babel-tangle)))
      (throw 'success nil))))

(defun docfiles/--publish (_ file _)
  "Tangle FILE and place the results wherever they say."
  (docfiles/tangle-file file))

(defun docfiles/--publish (_ file _)
  "Tangle FILE and place the results wherever they say."
  (org-babel-tangle-file file))

(defun docfiles/export (&optional force)
  "Set up an entire machine by tangling the whole docfiles directory.

      The universal argument will force export everything."
  (interactive "P")
  (setq org-confirm-babel-evaluate nil)
  (let ((saved-org-export-use-babel-value org-export-use-babel))
    (setq org-export-use-babel t)
    (org-publish-project
     `("docfiles"
       :use-babel t
       :exclude-tags ("noconfig")
       :base-directory ,docfiles/directory
       :publishing-directory "~/tmp"
       :recursive t
       :publishing-function docfiles/--publish)
     force)
    (setq org-export-use-babel saved-org-export-use-babel-value)))
;; tangle-file and publish:1 ends here

;; untangle
;; The functions ~docfiles/import-file~ and ~docfiles/import-directory~ are adapted this zeekat article:

;; https://zeekat.nl/articles/making-emacs-work-for-me.html

;; To get an existing config file or directory into an org note, we use this import functions.

;; Changes to zeekat's functions include the addition of my header args, a top level header, and using home-relative/absolute paths for files in the home directory.


;; [[file:../../notebook/docfiles/emacs/packages/docfiles.org::*untangle][untangle:1]]
(defun docfiles/import-file (path)
	 "Pull PATH into the current org document as a docfile."
	 (interactive "fFile to include: ")
	 (message "Untangling '%s'..." path)
	 (save-current-buffer
		(let ((lang (save-current-buffer
			 (set-buffer (find-file-noselect path))
			 (zeekat/mode->language major-mode))))
		  (insert (format "\n* %s\n#+begin_src %s :comments both :mkdirp yes :tangle %s\n"
				  (replace-regexp-in-string "\\[_-\\]" " " (file-name-base path))
				  lang
				  (replace-regexp-in-string "../../../" "~/" (file-relative-name path))))
		  (forward-char (cadr (insert-file-contents path)))
		  (insert "\n#+" "end_src\n"))))

	 (defun zeekat/->string (str)
		(cond
		 ((stringp str) str)
		 ((symbolp str) (symbol-name str))))

	 (defun zeekat/mode->language (mode)
		"Return the language for the given mode"
		(intern (replace-regexp-in-string "\\-mode$" "" (zeekat/->string mode))))

	 (defun docfiles/import-directory (path)
		"Recursively import PATH into the current file with `docfiles/import-file'."
		(interactive "Droot directory to untangle: ")
		(mapc (lambda (file)
			(docfiles/import-file file))
		 (cl-remove-if 'file-directory-p
			  (reverse (f-files path (lambda (p) t) t)))))
;; untangle:1 ends here

;; interupt-src-block

;; this function isn't really part of docfiles. it's here because it's really useful after you've imported a file and you want to hop through it, slice it into chunks and comment on them. I bind it to =H-<=.


;; [[file:../../notebook/docfiles/emacs/packages/docfiles.org::*interupt-src-block][interupt-src-block:1]]
(defun interupt-src-block nil
	 "Inject an end and a new beginning at this point in a babel src block"
	 (interactive)
	 (when (point-at-bol-or-indentation)
		(forward-line -1))
	 (end-of-line)
	 (let
		  ((babel-header
			 (save-excursion
				(re-search-backward "^#\\+begin_src ")
				;; get everything on the header line after "#+begin_src"
				(buffer-substring (+ (point) 11) (progn (end-of-line) (point))))))
		(insert "\n#+end_src\n\n#+begin_src" babel-header)
		(forward-line -1)))

(provide 'docfiles)
	 ;;; docfiles ends here
;; interupt-src-block:1 ends here

;; settings

;; We can define some settings with customize that will be used by file:../../../meta/library.org


;; [[file:../../notebook/docfiles/emacs/packages/docfiles.org::*settings][settings:1]]
(defcustom docfiles/deets
	'(name "chee"
		 email "yay@chee.party"
		 zodiac "pisces")
	"Settings that will be used by docfiles when the get-deets()
library-of-babel function is called."
	:type '(plist)
	:group 'docfiles)

(defun docfiles/get-deet (deet)
  (plist-get docfiles/deets deet))

(defmacro deet (deet)
  (docfiles/get-deet deet))
;; settings:1 ends here
