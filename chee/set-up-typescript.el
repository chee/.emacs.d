(provide 'set-up-typescript)

(add-to-list 'auto-mode-alist '("\\.[mc]?[tj]s\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[mc]?[tj]sx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.cts\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

(use-package tsdoc-highlight-mode)
