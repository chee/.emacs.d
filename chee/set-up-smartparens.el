;; :PROPERTIES:
;; :ID:       3712f27a-a116-4bef-9d69-b185fe1d1a50
;; :END:
;; #+title: smartparens setup



;; [[file:../../notebook/docfiles/emacs/setup/smartparens.org::+begin_src emacs-lisp :comments both :mkdirp yes :tangle ~/.emacs.d/chee/set-up-smartparens.el][No heading:1]]
;; (defun /sp-org-in-src-block-p (_id _action _context)
;; (org-in-src-block-p))

;; (defun /setup-smartparens-org-mode nil
;; 	(use-package smartparens-org
;; 		:elpaca nil)
;; 	(sp-with-modes 'org-mode
;; 		(sp-local-pair "\"" "\""
;; 			:unless '(/sp-org-in-src-block-p))
;; 		(sp-local-pair "'" "'"
;; 			:unless '(/sp-org-in-src-block-p))
;; 		(sp-local-pair "*" "*"
;; 			:unless '(sp-point-after-word-p
;; 					       sp-point-at-bol-p
;; 					       /sp-org-in-src-block-p)
;; 			:skip-match 'sp--org-skip-asterisk)
;; 		(sp-local-pair "_" "_"
;; 			:unless '(sp-point-after-word-p
;; 					       /sp-org-in-src-block-p))
;; 		(sp-local-pair "/" "/"
;; 			:unless '(sp-point-after-word-p
;; 					       sp-org-point-after-left-square-bracket-p
;; 					       /sp-org-in-src-block-p)
;; 			:post-handlers '(("[d1]" "SPC")))
;; 		(sp-local-pair "~" "~"
;; 			:unless '(sp-point-after-word-p
;; 					       /sp-org-in-src-block-p)
;; 			:post-handlers '(("[d1]" "SPC")))
;; 		(sp-local-pair "=" "="
;; 			:unless '(sp-point-after-word-p
;; 					       /sp-org-in-src-block-p)
;; 			:post-handlers '(("[d1]" "SPC")))
;; 		(sp-local-pair "«" "»")))
;; No heading:1 ends here


;; [[file:../../notebook/docfiles/emacs/setup/smartparens.org::+begin_src emacs-lisp :comments both :mkdirp yes :tangle ~/.emacs.d/chee/set-up-smartparens.el][No heading:2]]
(use-package smartparens
	:ensure t
	;; :hook
	;; (text-mode . smartparens-mode)
	;; (prog-mode . smartparens-strict-mode)
	:bind (:map smartparens-mode-map ("C-." . sp-rewrap-sexp)
			    ("C-<" . sp-backward-sexp)
			    ("C->" . sp-forward-sexp)
			    ("A-," . sp-select-previous-thing)
			    ("A-." . sp-select-next-thing)
			    ("<A-tab>" . sp-indent-defun)
			    ("C-." . sp-rewrap-sexp)
			    ("C-=" . sp-unwrap-sexp)
          ("C-s-t" . sp-transpose-sexp)
			    ("H-." . sp-select-next-thing)
			    ("C-(" . sp-backward-slurp-sexp)
			    ("M-(" . sp-backward-barf-sexp)
			    ("C-)" . sp-forward-slurp-sexp)
			    ("M-)" . sp-forward-barf-sexp)
			    ("s-<up>" . sp-beginning-of-sexp)
			    ("s-<down>" . sp-end-of-sexp)))



(provide 'set-up-smartparens)
;; No heading:2 ends here
