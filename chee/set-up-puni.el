;; puni
(defun chee/puni-unwrap-sexp (&optional open close)
  (interactive)
  (save-excursion
	  (let* ((bounds (puni-bounds-of-sexp-around-point))
			      (beg (+ (car bounds) 1))
			      (end (- (cdr bounds) 1)))
		  (puni-delete-region beg end 'kill)
		  (puni-backward-delete-char)
		  (if open (insert-char open))
		  (yank)
		  (if close (insert-char close)))))

(defun chee/puni-rewrap-sexp nil
  (interactive)
  (let ((open (read-char "Opening character? "))
	       (close (read-char "Closing character? ")))
	  (chee/puni-unwrap-sexp open close)))

(use-package puni :ensure t
  :config
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'emacs-lisp-mode-hook #'puni-mode)
  (add-hook 'lisp-data-mode-hook #'puni-mode)
  (unbind-key "C-S-k" puni-mode-map)
  :bind
  (:map puni-mode-map
    ("C-c C-<backspace>" . puni-force-delete)
    ("S-<backspace>" . puni-force-delete)
	  ("C-=" . chee/puni-unwrap-sexp)
	  ("C-." . chee/puni-rewrap-sexp)
	  ("C-M-t" . puni-transpose)
	  ("C-s-<up>" . puni-raise)
	  ("C-s-<left>" . puni-splice-killing-backward)
	  ("C-s-<right>" . puni-splice-killing-forward)
	  ("s-<backspace>" . puni-force-delete)
	  ("s-<kp-delete>" . puni-splice-killing-forward)

	  ("C-<left>" . puni-backward-sexp-or-up-list)
	  ("C-<right>" . puni-forward-sexp-or-up-list)
	  ("C-<up>" . (lambda nil (interactive)
					        (puni-beginning-of-sexp)
					        (forward-char -1)
					        (puni-beginning-of-sexp)))
	  ("C-<down>" . (lambda nil (interactive)
				            (puni-end-of-sexp)
				            (forward-char)
				            (puni-end-of-sexp)))
	  ("A-w" .
	    (lambda nil
		    (interactive)
		    (kill-region
		      (region-beginning)
		      (region-end))))

	  ("C-(" . puni-slurp-backward)
	  ("M-(" . puni-barf-backward)
	  ("C-)" . puni-slurp-forward)
	  ("M-)" . puni-barf-forward)
	  ("M-C-<up>" . puni-beginning-of-sexp)
	  ("M-C-<down>" . puni-end-of-sexp)))

(provide 'set-up-puni)
