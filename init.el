;;; -*- lexical-binding: t -*-
;;; emacs.d/init-el -- chee rabbits emacs config
;;; Commentary:
;; none
;;; Code:

(setq user-full-name "chee")
(setq user-mail-address "yay@chee.party")

;; Power and speed 🮲🮳
;; 👀
;; no gc until we’ve eaten our init
(setq gc-cons-threshold most-positive-fixnum)

(setq auto-mode-case-fold nil)

(setq inhibit-startup-screen t)

;; vv slightly slower but worth it to have an ever-present effervescent emacs
;; lisp buffer
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message " ")

(setq read-process-output-max (* 1024 1024))

;; thanks to doom for some of these ideas
(setq-default
  bidi-display-reordering 'left-to-right
  bidi-paragraph-direction 'left-to-right)

(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun crm-indicator (args)  "?"
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq frame-inhibit-implied-resize t)
(setq ffap-machine-p-known 'reject)

(setq gcmh-idle-delay 3
	gcmh-high-cons-threshold 1073741824
	gcmh-verbose nil
	gc-cons-percentage 0.6)

;; Throw away anything that's been customized using ~M-x customize~ &c. This way if
;; I want to make emacs work a certain way it has to be added to my docfiles
;; config.
(setq custom-file "/dev/null")

(setq apropos-do-all t)
(setq auth-sources (list "~/.authinfo.gpg"))

;; UTF-8
(prefer-coding-system 'utf-8)
(when (fboundp 'set-charset-priority) (set-charset-priority 'unicode))
(setq locale-coding-system 'utf-8)

;; Bootstrap package manager
;; https://github.com/progfolio/elpaca and [[https://github.com/jwiegley/use-package][use-package.el]]
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                        :ref nil :depth 1 :inherit ignore
                        :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                        :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
        (build (expand-file-name "elpaca/" elpaca-builds-directory))
        (order (cdr elpaca-order))
        (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
      (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                           (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                           "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
        (progn (message "%s" (buffer-string)) (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default nil))

(elpaca-wait)
(use-package async :ensure t)
(defun chee/process-elpaca-queues nil
  (interactive)
  (elpaca-process-queues))

;; Subwords
;; Subword mode lets you move through camelCase as if they are separate word, and also kebab and snake case
;; i really recommend this, but also a command to toggle it for when words need to be words
(global-subword-mode t)

;; 🗑️🦝
(setq delete-by-moving-to-trash nil)

;; Parens
;; - Highlight the matching paren.
;; - Highlight it immediately.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Regions
;; - When text is selected, put it in [[https://en.wikipedia.org/wiki/X_Window_selection#Selections][the primary selection]].
;; - When text is selected, typing something new replaces it.
(setq select-active-regions t)
(delete-selection-mode t)
(setq mouse-drag-and-drop-region t)
(setq mouse-drag-and-drop-region-cross-program t)

;; Be a friendly member of society
;; - Disable weird emacs files in the [[https://en.wikipedia.org/wiki/Working_directory][cwd]]
(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq
  auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

;; we love common lisp in this house
(use-package cl-lib :ensure nil)

;; Electricity ⚡
;; - Indent: Yes, it rules.
;; - Layout: No (maybe? (No))
;; - Pairs: OK! (but not in the minibuffer!!!)
;;   - update NO again because of liquid and org and others
;; - Quote: No
(electric-indent-mode t)
(electric-layout-mode -1)
(electric-pair-mode 1)
(setf electric-pair-inhibit-predicate
  (lambda (_)
    (or (minibufferp))))
(electric-quote-mode -1)

;; show me what function i'min
;;(which-function-mode t)

;; Text
;; When i do a git pull I’d like to see what’s new, you know?
(global-auto-revert-mode t)

(setq-default fill-column 80)

;; Mouse 🐁
(setq mouse-wheel-scroll-amount '(2 ((shift) . t)))
(setq mouse-wheel-progressive-speed nil)
;; scrolling on a window should scroll that window, even if ffm is off
(setq mouse-wheel-follow-mouse t)
;; think in pixels not in chars
(if (and (not mac-initialized)
      (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode 1))
(setf frame-resize-pixelwise t)
(setf window-resize-pixelwise t)
;; click on links in (almost) every buffer
;; (global-goto-address-mode nil)
(if (fboundp 'goto-address-at-mouse)
  (bind-key "M-s-<mouse-1>" #'goto-address-at-mouse))
;; (setq scroll-preserve-screen-position 'always)

;; Focus follows 🐭
(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)

;; visuals 👀
;;(global-tab-line-mode -1)
(tool-bar-mode -1)

;; the menubar doesn't take up room on a mac, it's the global top bar
(unless (equal system-type 'darwin)
  (menu-bar-mode -1))

;; be small and thin when you are guific
(when (display-graphic-p)
  (set-fringe-style (cons 4 2))
  (scroll-bar-mode -1))
(setq frame-title-format nil)
(setf ns-use-proxy-icon t)
(add-to-list 'default-frame-alist
  '(ns-appearance . light))
;; (add-to-list 'default-frame-alist
;;   '(undecorated-round . t))
(add-to-list 'default-frame-alist
  '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(fullscreen . nil))
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))



(setq tab-always-indent 'complete)
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq-default line-spacing 1)

;; theme
;; most theme packages are quite costly to load
;; this is nearly free
;; (load-theme 'lychee t)
(load-theme 'pale-rose t)

;; nevertheless,
;;(use-package gruvbox-theme :ensure t
;;  :config (load-theme 'gruvbox t))
;; (use-package doom-themes :ensure t
;; fuckin love dracula i do
;; :config (load-theme 'doom-dracula t))

;; i am a fancy little rabbit with a fancy little font
;; i'm a fancy rabbit and i have fancy needs
(set-face-attribute font-lock-comment-face
  nil :slant 'oblique)

;; Fonts
(defun +font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(defun +font-family (&rest names)
  (seq-find #'+font-available-p names))

(set-face-background 'child-frame-border "hotpink")

(if (display-graphic-p)
  ;; a nice font for editing and codeblocks
  (let ((faces '(default fixed-pitch)))
    (dolist
      (face faces)
      (set-face-attribute
	      face nil
	      :font (+font-family
		            "Fantasque Sans Mono"
		            "Iosevka Rabbits"
		            "Fira Code"
		            "Cascadia Code"
		            "IBM Plex Mono"
		            "Panic Sans"
		            "Monaco"
		            "Courier")
	      :weight 'normal
	      :width 'compressed
	      :height 160)))

  ;; pretty plain text
  (set-face-attribute
    'variable-pitch nil
    :family (+font-family
	            "Gill Sans"
	            "SF Pro"
	            "system-ui"
	            "Verdana"
	            "Trebuchet MS"
	            "Geneva"
	            "Helvetica"
	            "Arial")
    :height 180))

;; My packages
(defvar chee-directory (expand-file-name "chee" user-emacs-directory) "=^.^=")
(add-to-list 'load-path chee-directory)

(use-package cheebug)

;; Feline modeline
(use-package feline
  :ensure nil
  :config (feline-mode)
  :custom
  (feline-line-prefix "L")
  (feline-column-prefix "C")
  (feline-mode-symbols
	  '(emacs-lisp-mode "λ"
		   lisp-mode "Common Lisp"
		   python-mode "py"
		   typescript-mode "types"
		   tsx-ts-mode "typesx"
		   rustic-mode "🦀"
		   rust-mode "🦀"
		   zig-mode "🦎"
		   scheme-mode "Lisp-1")))

;; ahem.
;; These should probably be in the emacs block. So should the stuff in the "be normal" area, tbh.
;; making history
(savehist-mode t)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; say yes, briefly
(defalias 'yes-or-no-p 'y-or-n-p)
(defun noot nil "Just say yes" t)
(defun noop nil "Just say nil" (interactive))

(setq lisp-indent-function 'common-lisp-indent-function)
(setq-default lisp-indent-offset 2)
(setq native-comp-async-report-warnings-errors nil)

(unbind-key "C-<wheel-down>")
(unbind-key "C-<wheel-up>")

(add-to-list
  'auto-mode-alist
  '("PKGBUILD" . sh-mode))

(setq browse-url-browser-function 'browse-url-default-browser)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
		      (file-exists-p (byte-compile-dest-file buffer-file-name)))
	  (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(setq show-paren-context-when-offscreen t)

(defun auto-fill-comments-mode ()
  (interactive)
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'prog-mode-hook #'auto-fill-comments-mode)

(add-hook 'compilation-finish-functions
  (lambda (buf strg)
	  (let ((win  (get-buffer-window buf 'visible)))
		  (when win (delete-window win)))))

;; Environment variables
(setenv "PATH"
  (string-join
	  `("/opt/homebrew/bin"
       "/usr/local/bin" "/Users/chee/.local/bin" "/Users/chee/bin" "/home/chee/bin" "/Users/chee/bin"
		   ,(getenv "PATH"))
	  ":"))

(add-to-list 'exec-path "/usr/local/bin")

;; Expand region
(use-package expand-region :ensure t
	:bind ("C-c C-=" . er/expand-region)
	("C-c C--" . er/contract-region))

;; Emacs
;; This used to be a mode (~chee-mode~) with a map, but i can't remember why i did that exactly?
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(add-hook 'emacs-lisp-mode-hook
  (lambda nil
	  (setq-local lisp-indent-offset 2)
	  (setq-local indent-tabs-mode nil)
	  (setq-local tab-width 2)))

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)
(setq mac-right-command-modifier 'meta)
;; this is fun but impractical, new Fn-somethings become hardcoded in the OS
;; every version or so
;; (setq mac-function-modifier 'hyper)
(setq vc-follow-symlinks t)
;; todo move all these funcs elsewhere chee/text-operations.el or something
(defun point-at-bol-or-indentation nil
  "Check if point is at bol or bol-ish."
  (let ((point (point)))
	  (or (= point (point-at-bol))
		  (save-excursion (back-to-indentation) (= point (point))))))

(defun chee/back-to-indentation-or-beginning (point)
  (interactive "d")
  (back-to-indentation)
  (if (= point (point))
	  (beginning-of-line)))

(defun chee/reindent-buffer nil
  (interactive)
  (save-excursion
	  (set-mark (point-min))
	  (point-max)
	  (indent-for-tab-command)
	  (deactivate-mark)))

(defun shell-command-filter-region (start end command)
	(interactive
		(let (string)
			(unless (mark)
				(user-error "The mark is not set now, so there is no region"))
			(shell-command-on-region (region-beginning) (region-end)
				(read-shell-command "Shell command? ")
				nil t))))

(defun shell-command-print nil
	(interactive)
	(insert
		(with-temp-buffer
			(shell-command
				(read-shell-command "Shell command? ")
				(current-buffer))
			(buffer-string))))

(defun chee/scroll-or-bob (&optional arg)
	(interactive "P")
	(condition-case nil
		(if arg (scroll-down) (scroll-up))
		(error (if arg (goto-char (point-min))
						 (goto-char (point-max))))))
(defalias 'chee/page-down 'chee/scroll-or-bob)

(defun chee/page-up nil
	(interactive)
	(chee/scroll-or-bob t))

(defun chee/kill-region-or-word (start end &optional arg)
	(interactive "rN")
	(let ((kill-word (key-binding (kbd "M-d"))) (arg (or arg 1)))
		(let ((arg (or arg 1)))
			(if (and (mark) (region-active-p) (not (= start end)))
				(kill-region start end)
				(kill-word (- arg))))))


(defun chee/kill-region-or-line nil
  (interactive)
  (chee/with-region-or-line #'kill-region))

(defun chee/comment-or-uncomment-region-or-line ()
	(interactive)
	(chee/with-region-or-line 'comment-or-uncomment-region))

(defun chee/with-region-or-line (fn)
  "Eval FN (which takes beginning and end), with region if active,
  or with line as region."
  (let ((region-was-active (region-active-p)))
    (if (region-active-p)
      (progn
        (let ((start (region-beginning)) (end (region-end)))
          (funcall fn start end)
          (set-mark end)
          (goto-char start)))
      (funcall fn (line-beginning-position) (+ 1 (line-end-position))))))


(defun chee/indent-line-or-region nil
  (interactive)
  (let ((point (point)) (mark (mark)))
    (save-excursion
      (chee/with-region-or-line 'indent-rigidly-right-to-tab-stop))))

(defun chee/unindent-line-or-region nil
	(interactive)
	(chee/with-region-or-line 'indent-rigidly-left-to-tab-stop))

(defun chee+org-move-thing-up nil
  (interactive)
  (if (org-at-item-p)
	  (org-move-item-up)
	  (org-move-subtree-up)))
(defun chee+org-move-thing-down nil
  (interactive)
  (if (org-at-item-p)
	  (org-move-item-down)
	  (org-move-subtree-down)))

(defun chee/delete-horizontal-and-vertical-space nil
	(interactive)
	(call-interactively 'delete-horizontal-space)
  (call-interactively 'delete-blank-lines))


(bind-keys
  ("C-s-n" . make-frame)
  ("C-z" . ignore)
  ("s-<drag-mouse-1>" . mouse-set-region)
  ("<M-tab>" . other-window)
  ("<f5>" . eval-buffer) ;qbasic
  ("<home>" . chee/back-to-indentation-or-beginning)
  ("C-a" . chee/back-to-indentation-or-beginning)
  ("<end>" . end-of-line)
  ("C-v" . chee/page-down)
  ("M-v" . chee/page-up)
  ("M-;" . chee/comment-or-uncomment-region-or-line)
  ("C-<" . backward-sexp)
  ("C->" . forward-sexp)
  ("C-S-w" . kill-region)
  ("C-S-u" . insert-char)
  ("C-$" . shell-command-filter-region)
  ("C-h F" . describe-face)


  ;; nice
  ("M-SPC" . execute-extended-command)

  ("A-q" . quoted-insert)
  ("M-s-<left>" . windmove-left)
  ("M-s-<right>" . windmove-right)
  ("M-s-<up>" . windmove-up)
  ("M-s-<down>" . windmove-down)
  ("C-S-h" . windmove-left)
  ("C-S-j" . windmove-down)
  ("C-S-k" . windmove-up)
  ("C-S-l" . windmove-right)

  ;; macintosh bindings
  ("s-a" . mark-whole-buffer)
  ("s-c" . kill-ring-save)
  ("s-o" . project-find-file)
  ("s-F" . consult-ripgrep)
  ("s-q" . save-buffers-kill-terminal)
  ("s-s" . save-buffer)
  ("s-w" . kill-current-buffer)
  ("s-q" . (lambda nil (interactive)
             (condition-case nil (delete-frame)
               (error (save-buffers-kill-terminal)))))

  ("s-x" . chee/kill-region-or-line)
  ("s-{" . previous-buffer)
  ("s-}" . next-buffer)
  ("s-[" . chee/unindent-line-or-region)
  ("s-]" . chee/indent-line-or-region)
  ("s-8" . insert-char)
  ("s-v" . yank)

  ("s-k" . delete-window)

  ("C-s-s" . query-replace)
  ("C-S-s-s". query-replace-regexp)
  ("C-S-s". isearch-forward-regexp)

  :map emacs-lisp-mode-map)

(use-package cherries)

;; Panelize
;; Make a window sticky
(defun panelize-window (&optional window)
  "Tell WINDOW to behave like a panel.

This means it is strongly-dedicated to its buffer and does not
close when `delete-other-windows' (`C-x 1') is called."
  (interactive)
  (let ((win (or window (get-buffer-window))))
	  (set-window-parameter win 'no-delete-other-windows t)
	  (set-window-dedicated-p win t)))

(defun unpanelize-window (&optional window)
  "Tell WINDOW to stop acting like a panel.

It will no longer be dedicated, and it will close when
`delete-other-windows' is called."
  (interactive)
  (let ((win (or window (get-buffer-window))))
	  (set-window-parameter win 'no-delete-other-windows nil)
	  (set-window-dedicated-p win nil)))
;; Panelize:1 ends here

;; Bookmarks and sessions
;; OK. This is pretty nice.
(use-package helpful :ensure t
  :init
  (setq find-function-C-source-directory "~/soft/emacs/src/")
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h x" . #'helpful-command)
  ("C-h F" . #'helpful-function))

;; move-text
(use-package move-text :ensure t
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))

(defvar chee/solaire-disabled-buffer-names '("*scratch*"))

;; solaire
(use-package solaire-mode :ensure t
  :config
  (solaire-global-mode +1)

  (defun solaire-mode-real-buffer-p ()
    "Return t if the current buffer is a real (file-visiting) buffer."
    (or
      (seq-contains-p chee/solaire-disabled-buffer-names (buffer-name) #'string=)
      (bound-and-true-p org-src-mode)
      (buffer-file-name (buffer-base-buffer))))
  :custom-face
  (treemacs-window-background-face ((t (:inherit solaire-default-face))))
  (treemacs-hl-line-face ((t (:inherit solaire-hl-line-face)))))

;; scrintch
(use-package persistent-scratch
  :ensure t
  :config (persistent-scratch-autosave-mode t))

;; camels and snakes
(use-package string-inflection :ensure t
  :config
  (defun +ask-for-inflection nil
    (interactive)
    (let ((char (read-char-from-minibuffer "
pick an inflection any inflection:
[c] camelCase  [p] PascalCase
[s] snake_case [k] kebab-case
[u] UPPER_CASE
" "pucks")))
      (cl-case char
	      (?c (string-inflection-lower-camelcase))
	      (?p (string-inflection-camelcase))
	      (?s (string-inflection-underscore))
	      (?k (string-inflection-kebab-case))
	      (?u (string-inflection-upcase)))))
  (bind-key "C-'" #'+ask-for-inflection))

;; Expansions/snippets
(use-package tempel :ensure t
  :bind
  ("A-S-<tab>" . tempel-insert)
  ("A-<tab>" . tempel-complete)
  (:map tempel-map
	  ("A-<tab>" . tempel-next)
	  ("A-S-<tab>" . tempel-previous)
	  ("A-<return>" . tempel-done)
	  ("A-<escape>" . tempel-abort))
  :config
  (defun +tempel-refresh ()
    "Refresh abbrev mode to load new abbrevs."
    (interactive)
    (when tempel-abbrev-mode
      (tempel-abbrev-mode -1)
      (tempel-abbrev-mode 1)))
  (add-hook 'after-save-hook #'+tempel-refresh)
  (setq tempel-path
    (expand-file-name "templates/*.eld" user-emacs-directory))
  (defun +tempel-buffer-setup ()
    (interactive)
    (setq-local completion-at-point-functions
      (cons #'tempel-expand
	      completion-at-point-functions)))
  (add-hook 'prog-mode-hook #'+tempel-buffer-setup)
  (add-hook 'text-mode-hook #'+tempel-buffer-setup))
(use-package tempel-collection :ensure t
  :after tempel)

;; E-mail
(use-package set-up-email :ensure nil)

;; location
(use-package osx-location :ensure t
  :config
  (osx-location-watch)
  (when (eq system-type 'darwin)
    (add-hook 'osx-location-changed-hook
      (lambda ()
	      (setq calendar-latitude osx-location-latitude
	        calendar-longitude osx-location-longitude
	        calendar-location-name
	        (format "%s, %s" osx-location-latitude osx-location-longitude))))))

;; Avy
(use-package avy :ensure t
  :config
  (bind-keys
    :prefix-map +avy-prefix-map
    :prefix "H-."
    ("H-." . avy-goto-char)
    ("H-/" . avy-next)
    ("H-," . avy-prev)
    ("H-l" . avy-resume)
    ("w" . avy-goto-word-0)
    ("H-w" . avy-goto-word-1)
    ("H w" . avy-goto-subword-0)
    ("H-s s-w" . avy-goto-subword-1)
    ("H-;" . avy-pop-mark)))

;; Common Lisp
(use-package set-up-common-lisp :ensure nil)

;; Completions
(use-package set-up-completions :ensure nil)

(use-package set-up-puni :ensure nil)

;; Generic Files
;; add basic syntax highlighting for lots of files
(use-package generic-x :ensure nil)

;; .editorconfig
(use-package editorconfig-mode
  :ensure editorconfig
  :hook prog-mode text-mode
  :config (editorconfig-mode t))

;; Developer Documentation
;; This is nice, like Dash.app in Emacs. Will I remember I have it? It doesn't have common lisp docs. RIP.
(use-package devdocs :ensure t)
(use-package +emoji)


;; line numbers
;; - in programming modes only
;; - with 4 spaces, so it doesn't change size as yous scroll
(use-package display-line-numbers-mode :ensure nil
  :hook prog-mode html-mode)

;; eldoc
(use-package eldoc :ensure nil
  :config
  (defun +eldoc/buffer-mode nil
    (interactive)
    (panelize-window
      (display-buffer-in-direction
	      (eldoc-doc-buffer)
	      (list (cons 'direction 'right))))
    (setq eldoc-echo-area-prefer-doc-buffer t)))

(use-package eldoc-box :ensure t
  :bind
  ("s-/" . eldoc-box-help-at-point)
  ("C-s-/" . eldoc-box-hover-mode)
  :custom-face
  (eldoc-box-body ((t (:inherit fixed-pitch)))))

;; exec-path-from-shell
;; this is to make sure my $PATH in emacs matches my shell's path, so programs aren't unexpectedly missing.
(use-package exec-path-from-shell
	:ensure t
	:config (exec-path-from-shell-initialize)
	:custom (exec-path-from-shell-check-startup-files nil))

;; fic-mode
;; highlight TODO, FIXME and BUG. also KLUDGE, but
;; https://www.emacswiki.org/emacs/fic-mode.el
(use-package fic-mode
  :ensure t
  :hook prog-mode
  :custom-face
  (fic-face ((t (:background "#fff7f7" :foreground "#cc6699"))))
  :config
  (setq fic-highlighted-words
    '("FIXME" "TODO" "BUG" "HACK" "todo")))

;; flycheck
(use-package flycheck-mode
  :custom (flycheck-keymap-prefix (kbd "s-e"))
  :ensure flycheck
  :hook prog-mode
  :init
  (global-flycheck-mode t)
  :config
  (add-to-list
	  'flycheck-disabled-checkers
	  'emacs-lisp-checkdoc))
(use-package flycheck-pos-tip-mode
  :ensure flycheck-pos-tip

  :hook flycheck-mode)

(use-package tooltip
  :config
  (setq tooltip-delay 0.2)
  (setq tooltip-y-offset 5))

;; git
(use-package git-modes :ensure t)
(use-package git-timemachine :ensure t)
(use-package magit
  :ensure t
  :commands magit-status-mode
  :mode (("COMMIT_EDITMSG" . git-commit-mode))
  :config
  (setq with-editor-shell-command-use-emacsclient nil))

(use-package transient :ensure t)
(use-package forge :ensure t
  :after magit
  :config (use-package ghub :ensure t))

(use-package with-editor :ensure t
  :config (shell-command-with-editor-mode 1))

;; graphviz
(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")


;; zig
(use-package zig-mode :ensure t)

;; scheme
;; i prefer chicken, but guile runs everywhere
(use-package geiser-guile :ensure t)
(use-package macrostep-geiser :ensure t)
(use-package geiser :ensure t
  :config
  (setf geiser-scheme-implementation 'guile))

;; go
(use-package go-ts-mode :ensure nil
  :mode
  ("\\.go\\'" . go-ts-mode))

;; web-mode
(use-package web-mode :ensure t
  :mode "\\.hbs\\'" "\\.html\\'" "\\.webc\\'" "\\.liquid\\'" "\\.njk\\'" "\\.vue\\'"
  :hook (web-mode . (lambda nil
	                    ))
  :custom
  (web-mode-enable-optional-tags t)
  (web-mode-enable-front-matter-block t)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-element-content-fontification nil)
  (web-mode-enable-element-tag-fontification nil)
  (web-mode-enable-current-column-highlight nil)
  (web-mode-code-indent-offset tab-width "wide boi")
  (web-mode-css-indent-offset tab-width "wide boi")
  (web-mode-markup-indent-offset tab-width "wide boi"))

(add-hook 'editorconfig-custom-hooks
  (lambda (hash)
    (when (and (boundp 'web-mode-engine)
	          (equal web-mode-engine "vue"))
	    (setq web-mode-script-padding 0))))

(use-package set-up-ide)

;; nginx
(use-package nginx-mode :ensure t
	:mode "\\.nginx\\'" "nginx\\.conf\\'")

;; multiple cursors mode
;; multiple cursors mode is great, but it needs quite a bit of help to feel normal.
(defun /mc/bind nil
  (bind-keys
	  :map mc/keymap
	  ("s-u" . mc/unmark-next-like-this)
	  ("s-D" . mc/mark-previous-like-this)
	  ("s-U" . mc/unmark-previous-like-this)
	  ("s-F" . mc/mark-next-like-this)
	  ("<down-mouse-1>" . mc/keyboard-quit)
	  ("<mouse-1>" . mc/keyboard-quit)
	  ("s-k" . mc/skip-to-next-like-this)
	  ("s-K" . mc/skip-to-previous-like-this)
	  ("s-n" . mc/mark-next-like-this)
	  ("s-p" . mc/mark-previous-like-this)
	  ("<RET>" . newline)
	  ("<return>" . newline)
	  :prefix-map chee/multiple-cursors-map
	  :prefix "H-m"
	  ("S" . mc/mark-next-symbol-like-this)
	  ("s" . mc/mark-all-symbols-like-this)
	  ("W" . mc/mark-next-word-like-this)
	  ("w" . mc/mark-all-words-like-this)
	  ("d" . mc/mark-all-words-like-this-in-defun)
	  ("<down-mouse-1>" . mc/keyboard-quit))
  (remove-hook 'multiple-cursors-mode-hook '/mc/bind))

(use-package multiple-cursors
	:ensure t
	:init
	(bind-keys
	  ("s-d" . mc/mark-next-like-this)
	  ("s-<mouse-1>" . mc/add-cursor-on-click))
	(add-hook 'multiple-cursors-mode-hook '/mc/bind))

;; Markdown
(use-package set-up-markdown)

;; Rainbow
(use-package rainbow-mode :ensure t)

;; kind icons
(use-package kind-icon
  :ensure t
  :config
  (defun +use-kind-icons-for-corfu nil
    (setq kind-icon-default-face 'corfu-default)
    (setq corfu-margin-formatters (list #'kind-icon-margin-formatter)))
  (add-hook 'corfu-mode-hook #'+use-kind-icons-for-corfu))

;; project.el
(use-package project
  :config
  (use-package dired
	  :config
	  (setq dired-dwim-target t)
	  (setq dired-mouse-drag-files t)
	  (bind-key "f" 'project-find-file dired-mode-map))
  (defun chee/open-project nil
	  "Get a view of the project."
	  (interactive)
	  (dired (project-root (project-current))))
	;;(dirvish))
  (setq project-switch-commands 'chee/open-project)
  :bind-keymap ("s-p" . project-prefix-map) ("C-c C-p" . project-prefix-map)
  :bind
  ("C-s-p" . project-switch-project)
  (:map project-prefix-map
		("t" . eat-project)
		("v" . magit)
	  ("s-p" . project-switch-project)))

(defun chee/open-settings ()
  "Create or select the 'settings' frame and open the init file in it.
If already in the settings frame, hide it."
  (interactive)
  (let ((settings-frame (seq-find (lambda (frame)
                                    (string= (frame-parameter frame 'name) "settings"))
                          (frame-list)))
         (current-frame (selected-frame)))
    (cond
      ;; If we're already in the settings frame, hide it
      ((and settings-frame (eq current-frame settings-frame))
        (make-frame-invisible settings-frame))
      ;; If settings frame exists but we're not in it, select and raise it
      (settings-frame
        (raise-frame settings-frame)
        (select-frame settings-frame))
      ;; If no settings frame exists, create one
      (t
        (let ((new-frame (make-frame '((name . "settings")
                                        (fullscreen . nil)
                                        (width . 0.5)
                                        (height . 0.5)))))
          (select-frame new-frame)
          (find-file user-init-file))))))

(bind-key "s-," #'chee/open-settings)

;; apheleia / formatting / prettier etc
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  :after tramp)

;; restart-emacs
(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

;; stylesheets
(use-package scss-mode
	:ensure t
	:mode "\\.scss\\'"
  :init
  ;; this is gona, but scss mode uses it at the moment
  (setq flymake-allowed-file-name-masks nil))

(use-package set-up-puni)

;; licenses
(use-package lice :ensure t)

;; ssh conf
(use-package ssh-config-mode
  :ensure t
  :mode "ssh/config\\'")

;; csv
(use-package csv-mode :ensure t)

;; tramp
(use-package tramp
  :ensure nil
  :defer 0.5
  :config
  (connection-local-set-profile-variables
	  '/bin/bash
	  '((explicit-shell-file-name . "/bin/bash")
	     (explicit-bash-args . ("-i"))))

  (connection-local-set-profiles
	  '(:application tramp :protocol "ssh" :machine "shell.chee.party")
	  '/bin/bash)

  (connection-local-set-profiles
	  '(:application tramp :protocol "ssh" :machine "party")
	  '/bin/bash)

  :custom
  (vc-ignore-dir-regexp
	  (format "\\(%s\\)\\|\\(%s\\)"
		  vc-ignore-dir-regexp
		  tramp-file-name-regexp) "i will simply not use git with tramp" )
  (remote-file-name-inhibit-cache 180 "use the cached version for 2 mins")
  (tramp-use-ssh-controlmaster-options t)
  (tramp-verbose 0)
  (tramp-auto-save-directory (locate-user-emacs-file "tramp-save/"))
  (tramp-chunksize 2048)
  (tramp-default-method "sshx" "not scp! also use /bin/sh too"))

(use-package coverlay :ensure t)
(use-package css-in-js-mode
  :ensure (:host github :repo "orzechowskid/tree-sitter-css-in-js"))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;; undo tree
(use-package undo-tree
  :ensure t
  :commands undo-tree-undo undo-tree-redo
  :config (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist '(("." . "~/.undo")))
  :bind
  (("C-_" . 'undo-tree-undo)
	  ("M-_" . 'undo-tree-undo)
	  ("s-z" . 'undo-tree-undo)
	  ("s-Z" . 'undo-tree-redo)
	  ("s-y" . 'undo-tree-redo)))

(use-package vundo
  :ensure t
  :bind
  (("C-s-z" . 'vundo)))

(use-package set-up-eat)

(use-package ansi-color
  :config
  (defun chee/apply-ansi-colors ()
    "Apply ANSI color codes in the current buffer."
    (ansi-color-apply-on-region (point-min) (point-max)))

  (define-minor-mode chee/ansi-colors-mode
    "Minor mode to automatically display ANSI colors."
    :lighter " ANSICOLORS"
    (when chee/ansi-colors-mode
      (let ((inhibit-read-only t)
             (buffer-read-only nil))
        (chee/apply-ansi-colors)
        (set-buffer-modified-p nil))))

  (add-to-list 'auto-mode-alist '("\\.bench\\'" . (lambda () (chee/ansi-colors-mode 1)))))

;; which key
(use-package which-key
  :ensure t
  :bind ("s-?" . which-key-mode)
  :config
  (defun chee/which-key-delay (seq len)
	  "Delay certain sequences a little.
                Reason: it's distracting when pressing C-x C-s to see a popup because it happens automatically 100 times a minute."
	  (when (member seq '("C-x")) 0.5))
  (add-to-list 'which-key-delay-functions 'chee/which-key-delay)
  :custom (which-key-mode t) (which-key-idle-delay 0.01))

;; .cook files
(use-package cook-mode
  :ensure (cook-mode :host github :repo "cooklang/cook-mode"))

;; yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  (add-hook 'yaml-mode-hook
		(lambda ()
			(setq indent-tabs-mode nil)
			(setq tab-width 2)
			(setq yaml-indent-offset 2))))

(use-package evil :ensure t)


;; gcmh
(elpaca-wait)

(use-package gcmh
	:ensure t
	:init
	(setq gc-cons-threshold 100000000)
	(setq gc-cons-percentage 0.1)
	(gcmh-mode t))

(when (eq system-type 'darwin)
  (add-hook 'emacs-startup-hook
    (lambda () (do-applescript "tell application \"Emacs\" to activate"))))
