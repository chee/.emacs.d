;; -*- lexical-binding: t; -*-

;; cherries
;; my personal website :)
;; todo cherries mode
(defvar cherries-directory "~/web/chee.party/")
(defun cherries/number-within-range? (number range)
  (and (>= number (car range)) (<= number (cdr range))))

(defvar cherries/emoji-ranges
  '((9984 . 10175)
	   (126976 . 127023)
	   (127136 . 127231)
	   (127248 . 127386)
	   (127462 . 129479)))

(defun cherries/emoji? (char)
  (seq-some
	  (lambda (range)
		  (cherries/number-within-range? char range))
	  cherries/emoji-ranges))

(defun cherries/emoji-name-or-identity (char)
  "Return emoji name if CHAR is emoji, otherwise return CHAR."
  (if (cherries/emoji? char)
		(get-char-code-property char 'name)
	  (char-to-string char)))

(defun cherries/replace-emojis-with-names-in-string  (string)
  "Return a string that is STRING with emojis replaced by their names."
  (mapconcat 'cherries/emoji-name-or-identity (string-to-list string) ""))


(defun cherries/slugify (headline)
  "Create a slug from a title string."
  (replace-regexp-in-string
	  "-$" ""
	  (replace-regexp-in-string
		  "-+" "-"
		  (replace-regexp-in-string
		    "-:[:a-z0-9]+:$" ""
		    (replace-regexp-in-string
			    "[^a-z0-9+=~@]" "-"
			    (downcase
			      (cherries/replace-emojis-with-names-in-string headline)))))))

(defun cherries/title-to-filename (title &optional ext)
  (concat (cherries/slugify title) "." (or ext "md")))

(defun cherries/title-to-entry-path (title &optional ext)
  (expand-file-name
	  (concat
	    "documents/entries/"
	    (cherries/title-to-filename title ext))
	  cherries-directory))

(defun cherries/entry ()
  (interactive)
  (let ((title (read-string "title? ")))
	  (find-file (cherries/title-to-entry-path title))))

(defun cherries/weekly ()
  (interactive)
  (project-switch-project cherries-directory)
  (cd cherries-directory)
  (find-file (shell-command-to-string "npm run -s weekly"))
  (end-of-buffer)
  (insert "
"))

(defun cherries/note ()
  (interactive)
  (find-file
    (expand-file-name
	    (concat
	      "documents/entries/"
	      (format-time-string "%Y-"))
	    cherries-directory)))

(defun cherries/date ()
  (interactive)
  (insert "date: " (format-time-string "%FT%TZ")))

(defun rename-visited-md-to-txt ()
  (interactive)
  (rename-visited-file
    (replace-regexp-in-string "\\.md" ".txt" (buffer-file-name))))

(defun fill-whole-buffer ()
  (interactive)
  (fill-region (point-min) (point-max)))

(defun cherries/delete-wp-tags ()
  (interactive)
  (delete-matching-lines "tags\\|magicktitle\\|- \"wp\"\\|title:\\|^$"))

(defun cherries/migrate-wp-post ()
  (interactive)
  (goto-char (point-min))
  (cherries/delete-wp-tags)
  (fill-whole-buffer)
  (rename-visited-md-to-txt)
  (save-buffer))

(provide 'cherries)
