;;; md-mode --- a markdown mode with extras
;;; Commentary:
;;; be there for me
;;; Code:

(require 'polymode)
(require 'poly-lock)

(define-innermode poly-liquid-root-innermode
  :mode nil
  :fallback-mode 'host
  :head-mode 'host
  :tail-mode 'host)

(define-innermode poly-liquid-yaml-frontmatter-innermode poly-liquid-root-innermode
  :mode 'yaml-mode
  :head-matcher (pm-make-text-property-matcher 'markdown-yaml-metadata-start)
  :tail-matcher (pm-make-text-property-matcher 'markdown-yaml-metadata-end)
  :allow-nested nil)

(define-innermode poly-liquid-json-frontmatter-innermode poly-liquid-root-innermode
  :mode 'json-ts-mode
  :head-matcher "^---json$"
  :tail-matcher "^---$"
  :allow-nested nil)

(define-innermode poly-liquid-js-frontmatter-innermode poly-liquid-root-innermode
  :mode 'tsx-ts-mode
  :head-matcher "^---js$"
  :tail-matcher "^---$"
  :allow-nested nil)

(define-innermode poly-liquid-css-element-innermode poly-liquid-root-innermode
  "CSS <style/> block."
  :mode 'css-ts-mode
  :head-matcher "<style>"
  :tail-matcher "</style>"
  :allow-nested nil)

(define-innermode poly-liquid-css-bundle-innermode poly-liquid-root-innermode
  "CSS <style/> block."
  :mode 'css-ts-mode
  :head-matcher "{% stylesheet .*%}"
  :tail-matcher "{% endstylesheet %}"
  :allow-nested nil)

(define-innermode poly-liquid-js-element-innermode poly-liquid-root-innermode
  "JS <script/> block."
  :mode 'tsx-ts-mode
  :head-matcher "<script>"
  :tail-matcher "</script>"
  :allow-nested nil)

(define-innermode poly-liquid-js-bundle-innermode poly-liquid-root-innermode
  "JS <script/> block."
  :mode 'tsx-ts-mode
  :head-matcher "{% javascript .*%}"
  :tail-matcher "{% endjavascript %}"
  :allow-nested nil)

(define-hostmode poly-liquid-hostmode :mode 'web-mode)

;;;###autoload  (autoload 'poly-liquid-mode "md")
(define-polymode poly-liquid-mode
  :hostmode 'poly-liquid-hostmode
  :innermodes '(poly-liquid-yaml-frontmatter-innermode
                 poly-liquid-json-frontmatter-innermode
                 poly-liquid-js-frontmatter-innermode
                 poly-liquid-css-element-innermode
                 poly-liquid-js-element-innermode
                 poly-liquid-css-bundle-innermode
                 poly-liquid-js-bundle-innermode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . poly-liquid-mode))

(provide 'poly-liquid-mode)
;;; md ends here
