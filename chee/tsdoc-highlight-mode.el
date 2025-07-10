;;; tsdoc-highlight-mode.el --- Highlight TSDoc comments in TypeScript -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 0.6.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: faces, typescript

;;; Commentary:
;; A minor mode that highlights TSDoc comments (/** */) in TypeScript files.
;; Adapted from typescript-mode.el's JSDoc highlighting.

;;; Code:

(defgroup tsdoc-highlight nil
  "Highlight TSDoc comments in TypeScript."
  :group 'faces)

;; Faces
(defface tsdoc-tag-face
  '((t :inherit font-lock-keyword-face))
  "Face for TSDoc tags like @param, @returns."
  :group 'tsdoc-highlight)

(defface tsdoc-type-face
  '((t :inherit font-lock-type-face))
  "Face for TSDoc types in curly braces."
  :group 'tsdoc-highlight)

(defface tsdoc-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for TSDoc parameter names."
  :group 'tsdoc-highlight)

;; Regexp patterns adapted from typescript-mode
(defconst tsdoc-highlight-jsdoc-tag-re
  (concat "\\(?:^\\s-*\\*+\\s-*\\)?"  ; Leading * in multiline comments
    "\\(@"
    "\\(?:abstract\\|access\\|alias\\|api\\|arg\\|argument\\|async\\|augments\\|author\\|beta\\|borrows\\|bug\\|"
    "callback\\|category\\|class\\|classdesc\\|code\\|config\\|const\\|constant\\|constructor\\|constructs\\|copyright\\|"
    "default\\|defaultvalue\\|deprecated\\|desc\\|description\\|dict\\|emits\\|enum\\|event\\|example\\|exception\\|"
    "experimental\\|exports\\|extends\\|external\\|file\\|fileoverview\\|final\\|fires\\|for\\|func\\|function\\|"
    "generator\\|global\\|hidden\\|hideconstructor\\|host\\|ignore\\|implements\\|impltype\\|important\\|inherit\\|"
    "inheritdoc\\|inner\\|instance\\|interface\\|internal\\|kind\\|lends\\|license\\|link\\|linkcode\\|linkplain\\|"
    "listens\\|main\\|member\\|memberof\\|method\\|mixes\\|mixin\\|modifies\\|module\\|name\\|namespace\\|nocollapse\\|"
    "nosideeffects\\|note\\|override\\|overview\\|package\\|param\\|preserve\\|private\\|prop\\|property\\|protected\\|"
    "public\\|read\\|readonly\\|record\\|remarks\\|requires\\|return\\|returns\\|scope\\|see\\|since\\|static\\|"
    "struct\\|submodule\\|summary\\|template\\|this\\|throws\\|todo\\|tutorial\\|type\\|typedef\\|unrestricted\\|"
    "var\\|variation\\|version\\|virtual\\|writeonce\\|yield\\|yields"
    "\\)\\)\\b")
  "Regexp matching JSDoc tags.")

(defconst tsdoc-highlight-jsdoc-type-re
  "{\\([^}]+\\)}"
  "Regexp matching JSDoc types in curly braces.")

(defconst tsdoc-highlight-jsdoc-arg-name-re
  (concat tsdoc-highlight-jsdoc-tag-re
    "\\s-*"
    tsdoc-highlight-jsdoc-type-re
    "\\s-*\\[?\\([[:alnum:]_$\.]+\\)?\\]?")
  "Regexp matching JSDoc tags with type and parameter name.")

(defconst tsdoc-highlight-jsdoc-arg-tag-regexp
  (concat tsdoc-highlight-jsdoc-tag-re "\\s-*\\({[^}]+}\\s-*\\)?\\[?\\([[:alnum:]_$\.]+\\)?\\]?")
  "Regexp matching JSDoc parameter tags.")

;; Font-lock keywords
(defvar tsdoc-highlight-font-lock-keywords
  `((,tsdoc-highlight-jsdoc-arg-name-re
      (1 'tsdoc-tag-face t)
      (2 'tsdoc-type-face t)
      (3 'tsdoc-variable-face t t))
     (,tsdoc-highlight-jsdoc-tag-re
       (1 'tsdoc-tag-face t))
     (,tsdoc-highlight-jsdoc-type-re
       (1 'tsdoc-type-face t)))
  "Font lock keywords for TSDoc.")

(defvar-local tsdoc-highlight--keywords nil
  "Buffer-local font-lock keywords.")

;;;###autoload
(define-minor-mode tsdoc-highlight-mode
  "Minor mode to highlight TSDoc comments in TypeScript."
  :lighter " TSDoc"
  :group 'tsdoc-highlight
  (if tsdoc-highlight-mode
    (progn
      (setq tsdoc-highlight--keywords tsdoc-highlight-font-lock-keywords)
      (font-lock-add-keywords nil tsdoc-highlight--keywords 'append))
    (when tsdoc-highlight--keywords
      (font-lock-remove-keywords nil tsdoc-highlight--keywords)
      (setq tsdoc-highlight--keywords nil)))
  (font-lock-flush)
  (font-lock-ensure))

;;;###autoload
(defun tsdoc-highlight-setup ()
  "Setup tsdoc-highlight-mode for TypeScript files."
  (interactive)
  (dolist (hook '(typescript-mode-hook
                   typescript-ts-mode-hook
                   tsx-ts-mode-hook
                   js-mode-hook
                   js2-mode-hook))
    (add-hook hook #'tsdoc-highlight-mode)))

(provide 'tsdoc-highlight-mode)
;;; tsdoc-highlight-mode.el ends here
