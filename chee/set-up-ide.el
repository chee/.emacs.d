;;; -*- lexical-binding: t -*-
(provide 'set-up-ide)
(use-package cheebug)


(defvar chee/deno-project-files '("deno.json" "deno.jsonc" "deno.lock")
  "List of files that indicate we're in a Deno project.")

(defun chee/deno-dir-p (dir)
  "Check if DIR contains any Deno configuration files."
  (cl-some (lambda (file)
             (file-exists-p (expand-file-name file dir)))
    chee/deno-project-files))

(defun chee/find-containing-deno-config ()
  "Find Deno configuration files, stopping at project root if in a project."
  (when-let ((current-file (or (buffer-file-name) default-directory)))
    (let* ((project (project-current))
            (project-root (when project (project-root project)))
            (dir (file-name-directory current-file)))
      (locate-dominating-file dir
        (lambda (parent)
          (cond
            ((chee/deno-dir-p parent) parent)
            ((and project-root
               (string= (file-truename parent)
                 (file-truename project-root)))
              nil)
            (t nil)))))))

(defun chee/is-deno-project-p ()
  "Check if current file is in a Deno project."
  (let ((result (chee/find-containing-deno-config)))
    (and result (not (eq result 'stop)))))


(defun chee/activate-deno ()
  "Set the appropriate vars for Deno"
  (setq-local lsp-disabled-clients '(ts-ls))
  (cheebug "Activating Deno.")
  (lsp-register-custom-settings
    '(("deno.enable" t t)
       ("deno.lint" t t)))
  (cl-defmethod lsp-execute-command (_server (command (eql "deno.client.test")) arguments)
    "Handle deno.client.test command."
    (let* ((uri (aref arguments 0))
            (test-name (aref arguments 1))
            (options (aref arguments 2))
            (file-path (lsp--uri-to-path uri))
            (inspect (and options (gethash "inspect" options)))
            (cmd (if inspect
                   (format "deno test --inspect-brk --filter=\"%s\" \"%s\"" test-name file-path)
                   (format "deno test --filter=\"%s\" \"%s\"" test-name file-path))))
      (message "Running: %s" cmd)
      (compile cmd)))
  (let ((formatter
          (pcase major-mode
            ('js-mode 'denofmt-js)
            ('web-mode 'denofmt)
            ('typescript-ts-mode 'denofmt-ts)
            ('typescript-mode 'denofmt-ts)
            ('tsx-ts-mode 'denofmt-tsx)
            ('json-ts-mode 'denofmt-json)
            ('web-mode 'denofmt-html)
            ('css-ts-mode 'denofmt-css)
            ('markdown-mode 'denofmt-md))))
    (when formatter (setq-local apheleia-formatter formatter))))


(defun chee/configure-code-tools (&rest args)
  "Setup appropriate LSP server for TypeScript based on project type."
  (let ((result (chee/find-containing-deno-config)))
    (if result
      (chee/activate-deno)
      (progn
        (cheebug "Disabling deno because we aren't in a deno directory")
        (setq-local lsp-disabled-clients '(deno-ls))))))


(use-package lsp-mode
  :ensure t

  :init

  (setq lsp-keymap-prefix "s-l")
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-copilot-enabled nil)
  (setq lsp-lens-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-ui-doc-max-width 60)
  (setq lsp-ui-doc-border "black")
  (setq lsp-auto-execute-action nil)
  (setq lsp-json-schemas

    `[(:fileMatch ["*.css-data.json"]
        :url "https://raw.githubusercontent.com/Microsoft/vscode-css-languageservice/master/docs/customData.schema.json")
       (:fileMatch ["package.json"]
         :url "http://json.schemastore.org/package")
       (:fileMatch ["*.html-data.json"]
         :url "https://raw.githubusercontent.com/Microsoft/vscode-html-languageservice/master/docs/customData.schema.json")
       (:fileMatch ["*.schema.json"]
         :url "http://json-schema.org/draft-07/schema#")
       (:fileMatch ["bower.json"]
         :url "http://json.schemastore.org/bower")
       (:fileMatch ["composer.json"]
         :url "http://json.schemastore.org/composer")
       (:fileMatch ["tsconfig.json"]
         :url "http://json.schemastore.org/tsconfig")
       (:fileMatch ["tsconfig.*.json"]
         :url "http://json.schemastore.org/tsconfig")
       (:fileMatch ["typings.json"]
         :url "http://json.schemastore.org/typings")
       (:fileMatch [".bowerrc"]
         :url "http://json.schemastore.org/bowerrc")
       (:fileMatch [".babelrc"]
         :url "http://json.schemastore.org/babelrc")
       (:fileMatch [".babelrc.json"]
         :url "http://json.schemastore.org/babelrc")
       (:fileMatch ["babel.config.json"]
         :url "http://json.schemastore.org/babelrc")
       (:fileMatch ["jsconfig.json"]
         :url "http://json.schemastore.org/jsconfig")
       (:fileMatch ["jsconfig.*.json"]
         :url "http://json.schemastore.org/jsconfig")
       (:fileMatch ["project.json"]
         :url "http://json.schemastore.org/project")
       (:fileMatch ["omnisharp.json"]
         :url "http://json.schemastore.org/omnisharp")
       (:fileMatch [".eslintrc.json"]
         :url "http://json.schemastore.org/eslintrc")
       (:fileMatch [".eslintrc"]
         :url "http://json.schemastore.org/eslintrc")
       (:fileMatch ["biome.json"]
         :url "https://biomejs.dev/schemas/2.0.5/schema.json")
       (:fileMatch ["deno.json"]
         :url "https://raw.githubusercontent.com/denoland/deno/main/cli/schemas/config-file.v1.json")
       (:fileMatch ["deno.jsonc"]
         :url "https://raw.githubusercontent.com/denoland/deno/main/cli/schemas/config-file.v1.json")])
  :hook

  (tsx-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (typescript-mode . lsp)
  (json-ts-mode . lsp)
  :config
  (use-package lsp-biome)
  :commands lsp
  :bind (:map lsp-mode-map
          ("s-." . lsp-execute-code-action)
          ("<f12>" . lsp-find-definition)
          ("C-<f12>" . lsp-goto-implementation)))

(advice-add 'lsp :before #'chee/configure-code-tools)

(setq typescript-ts-mode-hook nil)

;; lol... https://github.com/emacs-lsp/lsp-mode/issues/3693#issuecomment-1525874078
(use-package yasnippet :ensure t :config (yas-global-mode))

(use-package lsp-ui :commands lsp-ui-mode :ensure t)

(use-package typescript-mode :ensure t
  :bind (:map typescript-mode-map ("C-c C-e" . deno-run-test-at-point)))


(use-package dap-mode :ensure t)


(defun deno-run-test-at-point ()
  "Run the Deno test at point and show output in a popup."
  (interactive)
  (bind-key "C-c C-e" 'deno-run-test-at-point 'typescript-ts-mode-map)
  (let* ((file (buffer-file-name))
          (test-name (deno--get-test-name-at-point))
          (cmd (if test-name
                 (format "deno test --filter=\"%s\" \"%s\" --no-check" test-name file)
                 (format "deno test \"%s\" --no-check" file))))
    (deno--run-and-show-output cmd)))

(defun deno--get-test-name-at-point ()
  "Extract test name from Deno.test() call at or before point."
  (save-excursion
    (let ((original-point (point))
           test-name)
      ;; First try the current line
      (beginning-of-line)
      (when (re-search-forward "Deno\\.test(\\s-*['\"`]\\([^'\"`,]+\\)" (line-end-position) t)
        (setq test-name (match-string 1)))

      ;; If not found, search backwards
      (unless test-name
        (goto-char original-point)
        (when (re-search-backward "Deno\\.test(\\s-*['\"`]\\([^'\"`,]+\\)" nil t)
          (setq test-name (match-string 1))))

      test-name)))

(defun deno--run-and-show-output (command)
  "Run COMMAND and show output in a child frame."
  (let* ((output-buffer "*Deno Test*")
          (process-buffer (get-buffer-create output-buffer)))
    (with-current-buffer process-buffer
      (chee/ansi-colors-mode)
      (erase-buffer)
      (insert (format "Running: %s\n\n" command))
      ;; Add keybinding to close the frame
      (local-set-key (kbd "q") 'deno--close-test-frame)
      (local-set-key (kbd "C-g") 'deno--close-test-frame))

    ;; Show in child frame immediately
    (deno--show-output-frame process-buffer)

    ;; Run the command asynchronously
    (let ((proc (start-process-shell-command
                  "deno-test" process-buffer command)))
      (set-process-sentinel proc 'deno--test-finished))))

(defun deno--test-finished (process event)
  "Handle test completion."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (with-current-buffer buffer
        (chee/ansi-colors-mode)
        (goto-char (point-max))
        (insert (format "\n\nProcess finished with: %s" (string-trim event)))
        (insert "\n\nPress 'q' or 'C-g' to close")))))

(defun deno--close-test-frame ()
  "Close the Deno test posframe."
  (interactive)
  (if (and (fboundp 'posframe-hide) (display-graphic-p))
    (posframe-hide "*Deno Test*")
    ;; For regular windows, just delete the window
    (when-let ((win (get-buffer-window "*Deno Test*")))
      (delete-window win))))

(defun deno--show-output-frame (buffer)
  "Show BUFFER in a child frame or popup window."
  (if (and (fboundp 'posframe-show) (display-graphic-p))
    ;; Use posframe if available (prettier)
    (posframe-show buffer
      :position (point)
      :width 80
      :height 20
      :min-width 80
      :min-height 10
      :border-width 2
      :border-color "orange"
      :accept-focus t)  ; Allow focus so keys work
    ;; Fallback to popup window
    (let ((win (display-buffer buffer
                 '((display-buffer-pop-up-window)
                    (window-height . 0.3)))))
      (with-selected-window win
        (goto-char (point-max))))))

;; Bind to a key
;; (eval-after-load 'typescript-ts-mode
;; (define-key 'typescript-ts-mode-map "C-c C-e" #'deno-run-test-at-point))

(use-package copilot :ensure t
  :config
  (add-to-list
    'copilot-major-mode-alist '("typescript-ts" . "typescript"))
  (add-to-list
    'copilot-major-mode-alist '("tsx-ts" . "typescriptreact"))

  (setq copilot-enable-predicates '(noop))
  (setq copilot-enable-display-predicates '(noot))
  (defun chee/copilot-turn-on-unless-haha-jk ()
    "Turn on `copilot-mode' if the buffer is writable."
    (unless (or buffer-read-only (minibufferp))
      (copilot-mode 1)))

  (define-global-minor-mode
    chee/global-copilot-mode
    copilot-mode chee/copilot-turn-on-unless-haha-jk)

  (chee/global-copilot-mode t)

  :bind (:map copilot-mode-map ("C-<return>" . copilot-complete))
  (:map copilot-completion-map
    ("C-<return>" . copilot-accept-completion)
    ("C-\\" . copilot-next-completion)))

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))

;; apheleia / formatting / prettier etc
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  (add-to-list 'apheleia-formatters
    '(denofmt-css "deno" "fmt" "-" "--ext" "css"))
  (add-to-list 'apheleia-formatters
    '(denofmt-html "deno" "fmt" "-" "--ext" "html"))
  :after tramp)


(use-package set-up-smartparens)
(use-package set-up-typescript)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package astro-ts-mode :ensure t
  :after treesit-auto lsp
  :init
  (add-hook 'astro-ts-mode-hook #'lsp)
  (let ((astro-recipe (make-treesit-auto-recipe
                        :lang 'astro
                        :ts-mode 'astro-ts-mode
                        :url "https://github.com/virchau13/tree-sitter-astro"
                        :revision "master"
                        :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe)))
