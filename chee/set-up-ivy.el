;; :PROPERTIES:
;; :ID:       3ecdfa8b-5add-47db-b893-8ccbbc3636d5
;; :END:
;; #+title: ivy


;; [[file:../../notebook/docfiles/emacs/setup/ivy.org::+begin_src emacs-lisp :comments both :mkdirp yes :tangle ~/.emacs.d/chee/set-up-ivy.el][No heading:1]]
(use-package ivy
		:elpaca t
		:defer 0.1
		:bind (("C-c C-r" . ivy-resume)
				 ("C-x B" . ivy-switch-buffer-other-window))
		:custom
		(ivy-count-format "(%d/%d) ")
		(ivy-use-virtual-buffers t)
		(enable-recursive-minibuffers t)
		(ivy-initial-inputs-alist
		 '((counsel-minor . "^+")
			(counsel-org-capture . "\\b")
			(counsel-M-x . "\\b")
			(counsel-describe-symbol . "\\b")
			(counsel-org-capture . "\\b")
			(org-refile . "\\b")
			(org-agenda-refile . "\\b")
			(org-capture-refile . "\\b")
			(woman . "\\b")
			(Man-completion-table . "\\b")))
		:config (ivy-mode))

(use-package counsel
		:elpaca t
		:after ivy
		:config (counsel-mode))

(use-package counsel-projectile
		:elpaca t
		:after ivy projectile
		:config (counsel-projectile-mode))

(use-package counsel-dash
		:after ivy
		:commands counsel-dash-at-point
		:bind (:map |power| ("d a p" . counsel-dash-at-point)))

(use-package ivy-pass
		:after ivy
		:elpaca t
		:bind (:map |power| ("s-p" . ivy-pass)))

(use-package swiper
		:elpaca t
		:after ivy
		:bind (("C-s" . swiper)
				 ("C-r" . swiper)))

(provide 'set-up-ivy)
;; No heading:1 ends here
