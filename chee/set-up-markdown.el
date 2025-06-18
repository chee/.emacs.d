;; #+title: Markdown

;; like markdown to use a ~variable-pitch~ font, with different font sizes for h1-h6, it's almost wsywig.


;; [[file:../../notebook/docfiles/emacs/setup/markdown.org::+begin_src emacs-lisp :comments both :mkdirp yes :tangle ~/.emacs.d/chee/set-up-markdown.el][No heading:1]]
(use-package md
	:ensure nil
  :after polymode
  :mode ("\\.md\\'" . md-mode)
	:bind (:map markdown-mode-map
			    ("s-b" . markdown-insert-bold)
			    ("s-i" . markdown-insert-italic)
			    ("s-C" . markdown-insert-code)
			    ("C-c C-'" . markdown-edit-code-block)
			    ("C-c i" . markdown-insert-image)
			    ("C-c TAB" . chee/interupt-code-block)
          ("C-a" . markdown-beginning-of-line)
          ("C-e" . markdown-end-of-line))
	:config
  (custom-set-faces
    '(markdown-blockquote-face
       ((t :inherits default)))
    '(markdown-header-face-1
       ((t (:height 1.9
             :weight normal))))
	  '(markdown-header-face-2
       ((t (:height 1.6
             :weight normal))))
	  '(markdown-header-face-3
       ((t (:height 1.5
             :weight normal))))
	  '(markdown-header-face-4
       ((t (:height 1.4
             :weight normal))))
	  '(markdown-header-face-5
       ((t (:height 1.3
             :weight normal))))
	  '(markdown-header-face-6
       ((t (:height 1.2
             :weight normal)))))

	(defun chee/interupt-code-block nil
		"Inject an end and a new beginning at this point in a markdown
							  code block."
		(interactive)
		(when (point-at-bol-or-indentation)
		  (forward-line -1))
		(end-of-line)
		(let
	    ((header
		     (save-excursion
			     (re-search-backward "^```")
			     ;; get everything on the header line after "```"
			     (buffer-substring (+ (point) 3) (progn (end-of-line) (point))))))
		  (insert "\n```\n\n```" header)
		  (forward-line -1)))

  (add-to-list 'markdown-uri-types "things")
	(add-hook 'markdown-mode-hook #'auto-fill-mode)
  :custom

  (markdown-hide-markup nil)
	(markdown-fontify-code-blocks-natively t)
	(markdown-header-scaling t)
	(markdown-enable-highlighting-syntax t)
	(markdown-hr-strings '("***" "---"))
  (markdown-special-ctrl-a/e t)
  (markdown-gfm-uppsetcase-checkbox t)
	(markdown-italic-underscore t)
  (markdown-link-space-sub-char "-")
  (markdown-wiki-link-alias-first nil)
  (markdown-wiki-link-fontify-missing t)
  (md-document-root "~/cherries/documents/z/"))

(use-package edit-indirect :ensure t)
(use-package polymode :ensure t)

(use-package js2-mode :ensure t
  :mode "\\.js2\\'")

(provide 'set-up-markdown)
;; No heading:1 ends here
