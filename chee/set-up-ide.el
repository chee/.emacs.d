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
  (let ((formatter
          (pcase major-mode
            ('js-mode 'denofmt-js)
            ('typescript-ts-mode 'denofmt-ts)
            ('tsx-ts-mode 'denofmt-tsx)
            ('json-ts-mode 'denofmt-json)
            ('markdown-mode 'denofmt-md))))
    (when formatter (setq-local apheleia-formatter formatter))))


(defun chee/configure-code-tools ()
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
  (setq lsp-copilot-enabled t)
  (setq lsp-lens-enable t)
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
       (:fileMatch ["deno.json"]
         :url "https://raw.githubusercontent.com/denoland/deno/main/cli/schemas/config-file.v1.json")
       (:fileMatch ["deno.jsonc"]
         :url "https://raw.githubusercontent.com/denoland/deno/main/cli/schemas/config-file.v1.json")])
  :hook

  (tsx-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (typescript-mode . lsp)
  (json-ts-mode . lsp)

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

(use-package dap-mode :ensure t)
