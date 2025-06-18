;;; pairing-mode --- be kinder to teammates
;;; Commentary:
;;; Enable and disable some features when pairing
;;; Code:

(defgroup pairing-mode nil
  "Pairing mode."
	:group 'mode-line
	:prefix "pairing-mode-"
	:link '(url-link :tag "Homepage" "https://github.com/chee/pairing-mode.el"))

(defgroup pairing-mode-faces nil
	"Pairing mode faces."
	:group 'feline
	:group 'faces)

(defun feline/mode-name (mode)
	"Get a feline representation of MODE."
	(replace-regexp-in-string "-mode$" "" (feline/get-mode-symbol mode)))

(defun feline/major-mode nil
	"Get a feline representation of the major mode."
	(feline/mode-name major-mode))

(defun feline/buffer-id (buffer-id)
	"Get a feline representation of BUFFER-ID."
	(propertize
	  (replace-regexp-in-string
		  "\\*Org Src \\([^[]*\\)\\[ \\(.*\\) +]"
		  " \\1→src(\\2)"
		  buffer-id)
	  'face 'feline-buffer-id-face))

(defun feline/evil nil
	"Get a feline representation of the evil mode."
	(when (bound-and-true-p evil-local-mode)
		(apply 'propertize
		  (cond
			  ((evil-normal-state-p) '("normal" face feline-evil-normal-face))
			  ((evil-emacs-state-p) '("emacs" face feline-evil-emacs-face))
			  ((evil-insert-state-p) '("insert" face feline-evil-insert-face))
			  ((evil-motion-state-p) '("motion" face feline-evil-motion-face))
			  ((evil-visual-state-p) '("visual" face feline-evil-visual-face))
			  ((evil-operator-state-p) '("operator" face feline-evil-operator-face))
			  ((evil-replace-state-p) '("replace" face feline-evil-replace-face))
			  (t ("normal" 'face 'feline-evil-normal-face))))))

(defun feline/positions nil
	"Present the line and column in the feline."
	(concat (propertize
		        feline-line-prefix
		        'face 'feline-position-prefix-face)
		(propertize
		  "%l"
		  'face 'feline-position-face)
		(propertize
		  feline-column-prefix
		  'face 'feline-position-prefix-face)
		(propertize
		  "%c"
		  'face 'feline-position-face)))

(defvar feline--project-name-cache '())

(defun feline/--last-meaningful-string (list)
	"Get last meaningful string from LIST."
	(let* ((n (car list)) (rest (cdr list)) (nn (car rest)))
		(if (or (not nn) (string-empty-p nn))
	    n
		  (feline/--last-meaningful-string rest))))

(defun feline/--project-name nil
	"Get the current project, if we're in one."
	(let ((project (project-current)))
		(when project
		  (feline/--last-meaningful-string
			  (split-string (project-root project) "/")))))

(defun feline/project-name nil
	"Get a feline representation of the current project name."
	(let* ((filename (buffer-file-name))
		      (cached (plist-get feline--project-name-cache filename)))
		(format
		  "(%s)"
		  (propertize
		    ;; tramp makes this _really slow_ so we're gonna just exit sideways
		    (if (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p filename))
		      "TRAMP"
	        (if cached cached
		        (or (feline/--project-name) "")))
		    'mouse-face 'mode-line-highlight
		    'local-map (let ((🐈‍⬛ (make-sparse-keymap)))
			               (define-key 🐈‍⬛ [mode-line mouse-1] 'project-switch-project)
			               🐈‍⬛)
		    'face 'feline-project-name-face))))

(defvar feline/original-modeline mode-line-format)

(defvar feline-mode nil "\"t\" if feline is active.")

(defun feline/spray nil
	"Stop feline, returning the previous modeline."
	(setq feline-mode nil)
	(setq-default mode-line-format feline/original-modeline))

(defun feline/purr nil
	"Start feline."
	(setq feline-mode t)
	(setq-default
	  mode-line-format
	  '(""
		   (:eval (feline/evil))
		   " "
		   (:eval (propertize (if (buffer-modified-p) "*" "") 'face 'feline-buffer-id-face))
		   (:eval (feline/major-mode))
		   " "
		   (:eval (feline/buffer-id (format-mode-line "%b")))
		   " "
		   (:eval (feline/project-name))
		   " "
		   (:eval (feline/positions))
		   " "
		   mode-line-misc-info)))

(define-minor-mode feline-mode
	"Toggle _f_e_l_i_n_e_."
	:group 'feline
	:global t
	:lighter nil
	(if feline-mode
		(feline/purr)
		(feline/spray)))

(provide 'feline)
;;; feline ends here
