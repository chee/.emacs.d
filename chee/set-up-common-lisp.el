;;; -*- lexical-binding: t -*-
(provide 'set-up-common-lisp)
;; Emacs
;; SLIME uses inferior-lisp variables when they are there.
(use-package inf-lisp
  :ensure nil
  :config
  (setq inferior-lisp-program "sbcl"))

;; corfu completions powered by SLIME
(defun +cape-slimy-rabbit (&optional interactive)
  (interactive)
  (require 'cape)
  (require 'slime)
  (pcase-let ((`(,beg . ,end) (cape--bounds 'symbol)))
    (when (eq (char-after beg) ?')
      (setq beg (1+ beg) end (max beg end)))
    `(,beg ,end
       ,(cape--table-with-properties
          (car (slime-contextual-completions beg end))
          :category 'slimy :sort nil)
       :annotation-function (lambda (_) " common lisp")
       :exclusive 'no)))

(use-package slime
  :ensure t
  :config
  (bind-keys
    :map help-map ("C-l" . hyperspec-lookup)
    :map lisp-mode-map
    ("<f5>" . slime-eval-buffer)
    ("s-;" . (lambda nil (interactive)
               (pop-to-buffer (slime-repl-buffer t))
               (goto-char (point-max)))))

  (defvar *chee/swank-defined-functions* nil
    "List of functions that were defined the last time we peeked in the lisp image.")
  (defun +swank-font-lock nil
    (interactive)
    (let* ((buffer-package (slime-find-buffer-package))
            (string (format "
        (cl:let ((common-lisp-externals
                (cl:loop
                  for sym being the external-symbols of (cl:find-package :cl)
                  when (cl:fboundp sym)
                  collect sym))
              fns)
          (cl:do-symbols (sym (cl:find-package :%s))
            (cl:when
             (cl:and
              (cl:fboundp sym)
              (cl:not (cl:member sym common-lisp-externals)))
            (cl:push sym fns)))
      fns)" buffer-package)))
      (slime-eval-async
        `(swank:eval-and-grab-output ,string)
        (lambda (result)
          (let
            ((defined-functions (car (read-from-string (cl-second result))))
              (keyword-face 'font-lock-keyword-face))
            ;; TODO split this into multiple shorter lists when big
            (dolist (function-group (seq-partition defined-functions 100))
              (let* ((function-names (mapcar
                                       (-compose #'downcase #'symbol-name)
                                       defined-functions))
                      (regex-ors (string-join function-names "\\|"))
                      (regex (format "(\\(%s \\)" regex-ors)))
                (font-lock-add-keywords 'lisp-mode `((,regex . (1 font-lock-keyword-face))))
                (lisp-mode))))))))

  (defvar +slime-completion-at-point-functions
    '(tempel-expand +cape-slimy-rabbit cape-file tags-completion-at-point-function))

  (defun +set-up-slime nil
    (interactive)
    (setq-local completion-at-point-functions
      (append +slime-completion-at-point-functions
        completion-at-point-functions))
    (slime-editing-mode t))

  (add-hook 'lisp-mode-hook #'+set-up-slime)
  (add-hook 'slime-mode-hook #'+set-up-slime)
  (add-hook 'slime-repl-mode-hook #'+set-up-slime)

  ;; (ql:quickload '(:asdf :alexandria :anaphora :drakma :dexador :fiveam
  ;; :closer-mop :iterate :do-urlencode :yason :html-entities :slite))
  ;; https://github.com/mmontone/slime-star
  ;;(add-to-list 'load-path (expand-file-name "aux/slime-star" org-directory))

  (setq slime-contribs
    '(slime-fancy slime-autodoc slime-asdf slime-quicklisp)
    slime-net-coding-system 'utf-8-unix)
  (setq common-lisp-style-default "sbcl")

  ;; Local Hyperspec
  (defun +nice-browse-url-function-you-have-here-it-would-be-a-shame-if-something-happened-to-it
    (orig-fun &rest args)
    (let ((browse-url-browser-function 'w3m-browse-url))
      (apply orig-fun args)))
  (advice-add 'hyperspec-lookup :around
    #'+nice-browse-url-function-you-have-here-it-would-be-a-shame-if-something-happened-to-it)
  (setq common-lisp-hyperspec-root
    (format "file://%s" (expand-file-name "common-lisp/HyperSpec/HyperSpec/" "~")))

  (mapc (lambda (msg) (push msg slime-words-of-encouragement))
    '("Come on, let's go."
       "Vamos."
       "A ver."
       "You'll always find me in the REPL at parties."
       "OK, let me have it.")))

(use-package w3m
  :ensure t
  :commands w3m-browse-url)

;; disabling due to its use of an obseleted function which creates an annoying
;; error during bytecomp lol
;;
;; (use-package erefactor :ensure t :bind (:map emacs-lisp-mode-map ("s-l r r"
;; . erefactor-rename-symbol-in-buffer) :map lisp-mode-map ("s-l r r"
;; . erefactor-rename-symbol-in-buffer)))

(use-package lisp-extra-font-lock
  :ensure t
  :hook (lisp-mode . lisp-extra-font-lock-mode))
;; Emacs:3 ends here

;; [[file:../../notebook/docfiles/emacs/setup/common-lisp.org::*Emacs][Emacs:4]]
(use-package slime-repl :ensure nil
  :after slime
  :config
  (bind-keys
    ("s-;" . (lambda nil (interactive) (previous-window-any-frame)))))
