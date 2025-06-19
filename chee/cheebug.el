;; -*- lexical-binding: t; -*-
;;; cheebug -- log to cheebug buffer

(provide 'cheebug)

(defun cheebug (format-string &rest args)
  "Log a message to the *cheebug* buffer.
Formats FORMAT-STRING interpolating ARGS."

  (with-current-buffer (get-buffer-create "*cheebug*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ")
      (apply #'format format-string args)
      "\n")))

(defun cheebug-show nil
  "Show the cheebug buffer."
  (interactive)
  (pop-to-buffer "*cheebug*"))
