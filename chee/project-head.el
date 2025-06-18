;; #+title: Project Head


;; [[file:../../notebook/docfiles/emacs/packages/project-head.org::+begin_src emacs-lisp :comments both :mkdirp yes :tangle ~/.emacs.d/chee/project-head.el][No heading:1]]
;; project-head --- Summary
;; Commentary:
;; Show the active project.el name in the headline
;; Code:

(provide 'project-head-mode)


(defgroup project-head nil
	 "Project headline")

(defgroup project-head-faces nil
	 "Project head's face"
	 :group 'project-head
	 :group 'faces
	 :link '(url-link :tag "Homepage" "https://chee.party/project-head"))

(define-minor-mode project-head-mode
	 "Show the active project.el name in the header."
	 nil
	 "ph"
	 project-head-mode-map)

(defun project-head-mode-on nil
	 "Activate project head"
	 (project-head-mode +1))

(define-global-minor-mode
	 global-project-head-mode
	 project-head-mode
	 project-head-mode-on)

(defface project-head
	 '((t
		 (:inherits 'default)
		 (:height 1.9)
		 (:background "#fcfffe")
		 (:foreground "#3399ff")))
	 "the face of the head"
	 :group 'project-head-faces)

(defun project-head--last-meaningful-string (list)
	 "Get last meaningful string from list"
	 (let* ((n (car list)) (rest (cdr list)) (nn (car rest)))
		(if (or (not nn) (string-empty-p nn))
	  n
		  (project-head--last-meaningful-string rest))))

(project-head--last-meaningful-string '("~" "spark" ""))

(defun project-head-project-name nil
	 (let ((project (project-current)))
		(when project
	  (project-head--last-meaningful-string
		(split-string (project-root project) "/")))))

(defun project-head--string-to-half-header-width (string &optional floor?)
	 (let ((width (quarter (string-width string))))
		(if (and (intern-soft floor?) (floor?))
	  (ceiling width)
		  (floor width))))

(defun project-head-rendered-name nil
	 (let* ((project-name (project-project-name))
		(half-name-head-width (string-to-half-header-width project-name))
		(spaces (make-string (- (quarter (window-width)) half-name-head-width) 32)))
		(concat spaces (project-head-project-name) spaces)))

(setq header-line-format
		  (setq-default
			header-line-format
			'(:eval
		(propertize (project-head-rendered-name) 'face 'project-head))))
;; No heading:1 ends here
