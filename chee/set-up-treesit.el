;;; set-up-treesit.el --- tree-sitter grammars & modes  -*- lexical-binding: t -*-
(provide 'set-up-treesit)
;;; Commentary:
;; Emacs 30 has everything needed to manage tree-sitter grammars on its own, so
;; there's no third-party layer here. Grammars are pinned to a tag, because an
;; ABI bump upstream breaks a mode silently and I'd rather find out when I
;; choose to bump than when I open a file.
;;
;; M-x chee/treesit-install-all   install whatever is missing
;; M-x chee/treesit-update-all    rebuild everything (after bumping a pin)
;; M-x chee/treesit-status        what's installed, what isn't
;;; Code:

(require 'treesit)

(defconst chee/treesit-grammars
  ;; (LANG URL REVISION SOURCE-DIR) — the positional form Emacs 30 expects.
  ;; The keyword form (:revision ...) is Emacs 31 and is silently ignored here.
  '((astro           "https://github.com/virchau13/tree-sitter-astro" "master")
     (bash           "https://github.com/tree-sitter/tree-sitter-bash" "v0.25.1")
     (css            "https://github.com/tree-sitter/tree-sitter-css" "v0.25.0")
     (dockerfile     "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
     (go             "https://github.com/tree-sitter/tree-sitter-go" "v0.25.0")
     (gomod          "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
     (html           "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
     (javascript     "https://github.com/tree-sitter/tree-sitter-javascript" "v0.25.0")
     (jsdoc          "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.25.0")
     (json           "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
     (markdown       "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.3"
       "tree-sitter-markdown/src")
     (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.3"
       "tree-sitter-markdown-inline/src")
     (regex          "https://github.com/tree-sitter/tree-sitter-regex" "v1.0.0")
     (rust           "https://github.com/tree-sitter/tree-sitter-rust" "v0.24.2")
     (toml           "https://github.com/tree-sitter-grammars/tree-sitter-toml" "v0.7.0")
     ;; tsx and typescript are two grammars in one repo; both lean on
     ;; ../../common/scanner.h, so the whole repo has to be cloned, not the subdir.
     (tsx            "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
     (typescript     "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
     (wast           "https://github.com/wasm-lsp/tree-sitter-wasm" "main" "wast/src")
     (wat            "https://github.com/wasm-lsp/tree-sitter-wasm" "main" "wat/src")
     (yaml           "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.2"))
  "Grammars this config actually has a mode for, pinned to a known-good tag.")

(setq treesit-language-source-alist (copy-tree chee/treesit-grammars))

;; css-in-js-mode ships its own grammar inside the package repo, so build it
;; from the checkout elpaca already made rather than cloning the same thing
;; twice. Resolved by path, not via `elpaca-get' — this file loads before
;; css-in-js-mode is queued, so the order lookup would come back empty.
(let ((repo (expand-file-name
              "tree-sitter-css-in-js"
              (or (bound-and-true-p elpaca-repos-directory)
                (expand-file-name "elpaca/repos/" user-emacs-directory)))))
  (when (file-directory-p (expand-file-name "src" repo))
    (setf (alist-get 'css-in-js treesit-language-source-alist) (list repo))))

;; Level 4 is everything the grammar's queries offer. The default, 3, leaves
;; things like operators and bracket matching unfontified.
(setq treesit-font-lock-level 4)

(dolist (remap '((css-mode        . css-ts-mode)
                  (js-mode        . js-ts-mode)
                  (javascript-mode . js-ts-mode)
                  (js-json-mode   . json-ts-mode)
                  (json-mode      . json-ts-mode)
                  (typescript-mode . typescript-ts-mode)
                  (sh-mode        . bash-ts-mode)
                  (conf-toml-mode . toml-ts-mode)))
  (add-to-list 'major-mode-remap-alist remap))

(defun chee/treesit-langs ()
  "Every language `treesit-language-source-alist' knows how to build."
  (mapcar #'car treesit-language-source-alist))

(defun chee/treesit-missing-langs ()
  "Languages we have a recipe for but no compiled grammar."
  (seq-remove #'treesit-language-available-p (chee/treesit-langs)))

(defun chee/treesit--install (langs)
  "Build each of LANGS, collecting failures instead of stopping at the first."
  (let (failed)
    (dolist (lang langs)
      (condition-case err
        (progn
          (message "[treesit] building %s..." lang)
          (treesit-install-language-grammar lang))
        (error
          (push (cons lang (error-message-string err)) failed)
          (message "[treesit] %s FAILED: %s" lang (error-message-string err)))))
    (if failed
      (message "[treesit] %d/%d failed: %s"
        (length failed) (length langs)
        (mapconcat (lambda (f) (symbol-name (car f))) (nreverse failed) ", "))
      (message "[treesit] %d grammar(s) ready" (length langs)))
    failed))

(defun chee/treesit-install-all ()
  "Build any grammar in `treesit-language-source-alist' that's missing."
  (interactive)
  (if-let* ((missing (chee/treesit-missing-langs)))
    (chee/treesit--install missing)
    (message "[treesit] nothing missing")))

(defun chee/treesit-update-all ()
  "Rebuild every grammar, even ones already installed.
This is the one to run after bumping a pin in `chee/treesit-grammars'."
  (interactive)
  (chee/treesit--install (chee/treesit-langs)))

(defun chee/treesit-status ()
  "Show which grammars are built and which aren't."
  (interactive)
  (with-current-buffer (get-buffer-create "*treesit status*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "tree-sitter ABI %s, grammars in %s\n\n"
                (treesit-library-abi-version)
                (or (car treesit-extra-load-path)
                  (locate-user-emacs-file "tree-sitter"))))
      (dolist (lang (sort (chee/treesit-langs) #'string<))
        (insert (format "  %-18s %s\n" lang
                  (if (treesit-language-available-p lang) "ok" "MISSING")))))
    (special-mode)
    (display-buffer (current-buffer))))

;; Don't prompt, don't download behind my back — just say so once.
(add-hook 'elpaca-after-init-hook
  (lambda ()
    (when-let* ((missing (chee/treesit-missing-langs)))
      (message "[treesit] %d grammar(s) missing (%s) — M-x chee/treesit-install-all"
        (length missing)
        (mapconcat #'symbol-name missing " ")))))

;;; set-up-treesit.el ends here
