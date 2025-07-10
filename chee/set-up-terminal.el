(defvar chee/last-eat-buffer-per-project (make-hash-table :test 'equal)
  "Hash table tracking last active eat buffer per project.")

(defun chee/remember-eat-buffer ()
  "Remember the current eat buffer for this project."
  (when (and (eq major-mode 'eat-mode)
          (chee/project-eat-buffer-p (buffer-name)))
    (when-let ((project-name (chee/safe-project-name)))
      (puthash project-name (current-buffer) chee/last-eat-buffer-per-project))))

(defun chee/get-eat-window ()
  "Get the window currently showing an eat buffer for this project."
  (seq-find (lambda (w)
              (with-current-buffer (window-buffer w)
                (and (eq major-mode 'eat-mode)
                  (chee/project-eat-buffer-p (buffer-name)))))
    (window-list)))

(defun chee/eat-other-window ()
  "Toggle eat window, remembering last active tab."
  (interactive)
  (if-let ((eat-window (chee/get-eat-window)))
    (progn
      (chee/remember-eat-buffer)
      (select-window eat-window))
    (if (project-current)
      (let* ((project-name (chee/safe-project-name))
              (last-buffer (gethash project-name chee/last-eat-buffer-per-project)))
        (if (and last-buffer (buffer-live-p last-buffer))
          (pop-to-buffer last-buffer)
          (eat-project-other-window t)))
      (eat-other-window))))

(defun chee/minimal-tab-line-format (tab tabs face _buffer-p selected-p)
  "Minimal tab format with no padding."
  (concat
    (if selected-p "[" " ")
    (buffer-name tab)
    (if selected-p "]" " ")))


(setq tab-line-separator "")
(setq tab-line-tab-min-width 1)
(setq tab-line-tab-max-width 30)

;; Remember the buffer when switching tabs
(add-hook 'buffer-list-update-hook #'chee/remember-eat-buffer)

(defun chee/safe-project-name nil
  (when (project-current)
    (project-name (project-current))))

(defun chee/project-eat-buffer-p (buffer-name &rest args)
  "Check if buffer is a project eat buffer."
  (when-let ((project (project-current)))
    (let ((project-eat-pattern (format "\\*%s-eat\\*" (project-name project))))
      (string-match-p (concat project-eat-pattern "\\(?:<[0-9][0-9]?+>\\)?") buffer-name))))

(defun chee/project-eat-tabs-function ()
  "Return current project's eat buffers in sorted order."
  (when-let ((project (project-current)))
    (let ((project-eat-pattern (format "\\*%s-eat\\*" (project-name project))))
      (sort (seq-filter (lambda (buf)
                          (string-match-p (concat project-eat-pattern "\\(?:<[0-9]+>\\)?")
                            (buffer-name buf)))
              (buffer-list))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))))

(defun chee/setup-project-eat-tabs ()
  "Set up tab-line for project eat buffers."
  (when (chee/project-eat-buffer-p (buffer-name nil))
    (setq-local tab-line-tabs-function #'chee/project-eat-tabs-function)
    (setq-local tab-line-tabs-buffer-group-sort-function nil)
    (setq-local tab-line-tabs-sort-function nil)
    (tab-line-mode 1)
    (chee/remember-eat-buffer)))

(defun chee/tab-line-next-tab ()
  "Switch to next tab in the current tab-line."
  (interactive)
  (when tab-line-tabs-function
    (let* ((tabs (funcall tab-line-tabs-function))
            (current-buf (current-buffer))
            (current-index (cl-position current-buf tabs))
            (next-index (if current-index
                          (mod (1+ current-index) (length tabs))
                          0)))
      (when (and tabs (> (length tabs) 0))
        (switch-to-buffer (nth next-index tabs))))))

(defun chee/tab-line-prev-tab ()
  "Switch to previous tab in the current tab-line."
  (interactive)
  (when tab-line-tabs-function
    (let* ((tabs (funcall tab-line-tabs-function))
            (current-buf (current-buffer))
            (current-index (cl-position current-buf tabs))
            (prev-index (if current-index
                          (mod (1- current-index) (length tabs))
                          (1- (length tabs)))))
      (when (and tabs (> (length tabs) 0))
        (switch-to-buffer (nth prev-index tabs))))))

(defun chee/display-eat-buffer (buffer alist)
  "Custom display function for eat buffers."
  (let ((window (get-buffer-window buffer 'visible)))
    (if window

      (select-window window)

      (let ((eat-window (seq-find
                          (lambda (w)
                            (with-current-buffer (window-buffer w)
                              (and (eq major-mode 'eat-mode)
                                (chee/project-eat-buffer-p (buffer-name) nil))))
                          (window-list))))
        (if eat-window
          (set-window-buffer eat-window buffer)
          (display-buffer-at-bottom buffer alist))))))


(defun chee/eat-new-tab nil
  (interactive)
  (switch-to-buffer
    (eat-make
      (format "%s:%s" (or (chee/safe-project-name) "eat") (gensym))
      (getenv "SHELL"))))

;; Remember the buffer when switching tabs
(add-hook 'buffer-list-update-hook #'chee/remember-eat-buffer)

(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)
(setq tab-line-separator "")
(custom-set-faces
  '(tab-line ((t (:background "white" :foreground "black" :height 1.0 :box nil :padding 0))))
  '(tab-line-tab ((t (:background "black" :foreground "white" :box nil :padding 0))))
  '(tab-line-tab-current ((t (:background "black" :foreground "white"  :box nil :padding 0))))
  '(tab-line-tab-inactive ((t (:background "white" :foreground "black"  :box nil :padding 0))))
  '(tab-line-highlight ((t (:background "pink"  :box nil :padding 0)))))

(setq tab-line-tab-face-functions
  nil)

(setq tab-line-tab-name-function #'tab-line-tab-name-buffer)


;; eat
(use-package eat
  :ensure
  (:type git
	  :host codeberg
	  :repo "akib/emacs-eat"
	  :files ("*.el" ("term" "term/*.el") "*.texi"
		         "*.ti" ("terminfo/e" "terminfo/e/*")
		         ("terminfo/65" "terminfo/65/*")
		         ("integration" "integration/*")
		         (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (add-to-list
    'display-buffer-alist
    '(chee/project-eat-buffer-p
       (chee/display-eat-buffer)
       (direction . bottom)
       (inhibit-switch-frame . t)
       (window-height . 0.35)
       (reusable-frames . visible)
       (window-parameters . ((no-other-window . t)))))


  (defun my/eat-tabs-function ()
    "Return only eat buffers for tab-line."
    (seq-filter (lambda (buf)
                  (string-match-p "\\*.*eat.*\\*" (buffer-name buf)))
      (buffer-list)))

  (add-hook 'eat-mode-hook #'chee/setup-project-eat-tabs)




  :bind ("s-j" . chee/eat-other-window)
  (:map
    eat-mode-map
	  ("C-c C-x" . vterm-send-C-x)
	  ("C-c M-x" . vterm-send-C-x)
	  ("s-v" . eat-yank)
	  ("s-c" . kill-ring-save)
    ("s-}" . chee/tab-line-next-tab)
    ("s-{" . chee/tab-line-prev-tab)
    ("s-t" . (lambda nil (interactive) (eat-project t)))
    ("s-w" . (lambda nil (interactive) (let ((tab (current-buffer))) (chee/tab-line-next-tab) (kill-buffer tab))))
    ("s-j" . delete-window)))

(provide 'set-up-terminal)
