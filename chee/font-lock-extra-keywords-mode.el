;;; font-lock-extra-keywords-mode
;;; Commentary:
;;; Extra keywords for for font-lock
;;; Code:

(defgroup font-lock-extra-keywords nil
  "Extra keywords for `font-lock'."
  :group 'faces
  :prefix "font-lock-extra-keywords-"
  :link '(url-link :tag "Homepage" "https://github.com/chee/font-lock-extra-keywords-mode"))

(defcustom font-lock-extra-keywords-alist nil
  "Alist representing extra keywords to add.
`car' is the mode symbol, while `cdr' is the form expected by
`font-lock-add-keywords'."
  :type '(sexp)
  :group 'font-lock-extra-keywords)

(defvar font-lock-extra-keywords-mode nil
  "\"t\" if font-lock-extra-keywords is active.")

(defun font-lock-extra-keywords--remove (cons)
  (font-lock-remove-keywords (car cons) (cdr cons)))

(defun font-lock-extra-keywords--add (cons)
  (font-lock-add-keywords (car cons) (cdr cons)))

(defun font-lock-extra-keywords-die nil
  "Stop font-lock-extra-keywords highlighting."
  (interactive)
  (mapc #'font-lock-extra-keywords--remove font-lock-extra-keywords-alist)
  (setq font-lock-extra-keywords-mode nil))

(defun font-lock-extra-keywords-live nil
  "Start font-lock-extra-keywords highlighting."
  (interactive)
  (mapc #'font-lock-extra-keywords--add font-lock-extra-keywords-alist)
  (setq font-lock-extra-keywords-mode t)
  (font-lock-update))

(define-minor-mode font-lock-extra-keywords-mode
  "Toggle font-lock-extra-keywords."
  :group 'font-lock-extra-keywords
  :global t
  :lighter nil
  (if font-lock-extra-keywords-mode
    (font-lock-extra-keywords-die)
    (font-lock-extra-keywords-live)))

(provide 'font-lock-extra-keywords-mode)
;;; font-lock-extra-keywords-mode ends here
