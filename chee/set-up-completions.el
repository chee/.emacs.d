;; vertico

;; [[file:../../notebook/docfiles/emacs/setup/completions-vertico-consult-corfu-cape.org::*vertico][vertico:1]]
(use-package vertico
	:ensure t
	:defer 0.1
	:bind (:map vertico-map
			    ("?" . #'minibuffer-completion-help)
			    ("M-RET" . #'minibuffer-force-complete-and-exit)
			    ("M-TAB" . #'minibuffer-complete)
			    ("C-v" . #'vertico-scroll-up)
			    ("M-v" . #'vertico-scroll-down))
	:config

	(setq vertico-resize nil
	  vertico-count 20
	  vertico-cycle t
	  completion-in-region-function (lambda (&rest args)
					                          (apply (if vertico-mode
							                               #'consult-completion-in-region
							                               #'completion--in-region)
								                      args))
	  vertico-sort-function #'vertico-sort-history-length-alpha)
	(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
	(vertico-mode t))
;; vertico:1 ends here

;; [[file:../../notebook/docfiles/emacs/setup/completions-vertico-consult-corfu-cape.org::*vertico][vertico:2]]
(use-package consult
	:ensure t
	:bind (("C-x b" . consult-buffer)
		      ("M-y" . consult-yank-pop)
          ("M-g g" . consult-mark)
		      ("M-g m" . consult-mark)
		      ("M-g k" . consult-global-mark)
		      ("M-g i" . consult-imenu)
		      ("M-g I" . consult-imenu-multi)
		      ("M-s e" . consult-isearch-history)
		      ("M-g f" . consult-flycheck)
		      :prefix-map chee/consult-map
		      :prefix "s-f"
		      ("d m" . consult-man)
		      ("m m" . consult-mark)
		      ("m g" . consult-globgal-mark)
		      ("y" . consult-yank-pop)
		      ("f" . consult-find)
		      ("F" . consult-locate)
		      ("s G" . consult-grep)
		      ("s g" . consult-git-grep)
		      ("s r" . consult-ripgrep)
          ("s d" . @consult-ripgrep-in-directory)
		      ("l l" . consult-line)
		      ("l g" . consult-goto-line)
		      ("l m" . consult-line-multi)
		      ("o m" . consult-multi-occur)
		      ("l k" . consult-keep-lines)
		      ("l f" . consult-focus-lines)
		      ("h" . consult-history)
		      ("M" . consult-mode-command)
		      ("K" . consult-kmacro)
		      (":" . consult-complex-command)
		      ("t" . consult-theme)
		      ("b o w" . consult-buffer-other-window)
		      ("b o f" . consult-buffer-other-frame)
		      ("b k" . consult-bookmark)
		      :map isearch-mode-map
		      ("M-e" . consult-isearch-history)
		      ("M-s e" . consult-isearch-history)
		      ("M-s l" . consult-line)
		      ("M-s L" . consult-line-multi)
		      :map project-prefix-map
		      ("b" . consult-project-buffer)
		      ("s" . consult-ripgrep)
		      ("g" . consult-git-grep)
		      ("G" . consult-grep))
	:hook (completion-list-mode . consult-preview-at-point-mode)
	:init (setq register-preview-delay 0.2
			    register-preview-function #'consult-register-format)
	(setq xref-show-xrefs-function #'consult-xref
	  xref-show-definitions-function #'consult-xref)
	(advice-add #'register-preview :override #'consult-register-window)
	:config
  (defun @consult-ripgrep-in-directory nil
    (interactive)
    (consult-ripgrep (read-directory-name "Ripgrep where? ")))
	(setq consult-locate-args "mdfind")
	(setq consult-narrow-key "<")
	(consult-customize
	  consult-line
	  :add-history (seq-some #'thing-at-point '(region symbol)))
	(consult-customize
	  consult-ripgrep consult-git-grep consult-grep
	  consult-bookmark consult-recent-file
	  consult--source-recent-file consult--source-project-recent-file   consult--source-bookmark
	  :preview-key "s-f p"))

(use-package consult-flycheck :ensure t :after flycheck)
(use-package embark :ensure t
  :after consult
  :bind (:map chee/consult-map
          ("." . embark-act)
          ("s-." . embark-act-all))
  (:map minibuffer-mode-map
    ("s-e" . embark-export)))
(use-package embark-consult :ensure t)
;; (use-package marginalia :ensure t
;; :config (marginalia-mode t))
(use-package wgrep :ensure t)

(use-package savehist
	:ensure nil
	:init
	(savehist-mode))

(use-package orderless
	:ensure t
	:init
	;; Configure a custom style dispatcher (see the Consult wiki)
	(setq orderless-style-dispatchers '()
	  orderless-component-separator #'orderless-escapable-split-on-space
	  completion-styles '(orderless flex)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles partial-completion)))))
;; vertico:2 ends here

;; Corfu


;; [[file:../../notebook/docfiles/emacs/setup/completions-vertico-consult-corfu-cape.org::*Corfu][Corfu:1]]
(use-package corfu
  :ensure t
  :bind (:map corfu-map
          ("RET" . corfu-insert))
  :config

  (defun corfu-insert (arg)
    "Insert current candidate.
Quit (and insert newline) if no candidate is selected."
    (interactive "P")
    (if (>= corfu--index 0)
      (corfu--insert 'finished)
      (progn
        (corfu-quit)
        (newline arg t))))

  (unless (display-graphic-p)
    (use-package popon :ensure '(popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))
    (use-package corfu-terminal
      :if (not (display-graphic-p))
      :ensure '(corfu-terminal
                 :type git
                 :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
      :config (corfu-terminal-mode +1)))
  :custom
  (corfu-preview-current nil)
  (corfu-auto-prefix 1)
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)               ;; Enable auto completion
  (corfu-separator ?\s)        ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect-first t)    ;; Disable candidate preselection
  (corfu-on-exact-match t)     ;; Configure handling of exact matches
  (corfu-echo-documentation t) ;; Enable documentation in the echo area
  (corfu-scroll-margin 5)      ;; Use scroll margin
  :init
  (global-corfu-mode))
;; Corfu:1 ends here

;; cape


;; [[file:../../notebook/docfiles/emacs/setup/completions-vertico-consult-corfu-cape.org::*cape][cape:1]]
(use-package cape
	:ensure t
	:bind (
		      :prefix-map chee/cape
		      :prefix "C-c C-l"
		      ("p" . completion-at-point)
		      ("t" . complete-tag)
		      ("d" . cape-dabbrev)
		      ("h" . cape-history)
		      ("f" . cape-file)
		      ("k" . cape-keyword)
		      ("s" . cape-symbol)
		      ("a" . cape-abbrev)
		      ("i" . cape-ispell)
		      ("l" . cape-line)
		      ("w" . cape-dict)
		      ("\\" . cape-tex)
		      ("_" . cape-tex)
		      ("^" . cape-tex)
		      ("&" . cape-sgml)
		      ("r" . cape-rfc1345))
	:init
	;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
	;;(add-to-list 'completion-at-point-functions #'cape-symbol)
	;;(add-to-list 'completion-at-point-functions #'cape-dict)
	;; (add-to-list 'completion-at-point-functions #'cape-history)
	;; (add-to-list 'completion-at-point-functions #'cape-line)
	;;(add-to-list 'completion-at-point-functions #'cape-keyword)
	;;(add-to-list 'completion-at-point-functions #'cape-tex)
	;;(add-to-list 'completion-at-point-functions #'cape-sgml)
	;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
	;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
	;;(add-to-list 'completion-at-point-functions #'cape-ispell)
	(add-to-list 'completion-at-point-functions #'cape-file))
;; cape:1 ends here

;; Provider

;; [[file:../../notebook/docfiles/emacs/setup/completions-vertico-consult-corfu-cape.org::*Provider][Provider:1]]
(provide 'set-up-completions)
;; Provider:1 ends here
