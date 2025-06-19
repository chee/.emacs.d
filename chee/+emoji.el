(defgroup +emoji nil
  "We get emoji."
  :group 'ui)

(defcustom +emoji/fontname
  "Apple Color Emoji"
  "The emoji to use for the emoji block.  Pick something with nice animals."
  :type 'string
  :group '+emoji)

;; iosevka is the default because it has the wonderful legacy computing block,
;; starring LEFT HALF RUNNING MAN 🮲🮳
(defcustom +emoji/fallback-fontname
  "Iosevka"
  "The emoji to use as the fallback font.  Pick something with LEFT HALF RUNNING MAN."
  :type 'string
  :group '+emoji)

(defconst +emoji/ranges
  '((#x2620 . #x263f) ; some other things
	   (#x2700 . #x27bf) ; dingbats
	   (#x1f000 . #x1f02f) ; mahjong
	   (#x1f0a0 . #x1f19b) ; cards and more
	   ;; and just about everything else. big dingbat energy
	   (#x1f1e6 . #x1fad7))
  "These ranges were handcrafted.  They might be wrong.")

(defun +emoji/register-font-for-range (font-name &optional range)
  "Set FONT-NAME as the fontset font for unicode codepoint RANGE."
  (set-fontset-font "fontset-default"
		range
		(font-spec
			:name (format "%s:" font-name)
			:registry "iso10646-1")))

(defun +emoji/register-fallback nil
  "Use `+emoji/fallback-fontname' as the fallback font."
  (+emoji/register-font-for-range +emoji/fallback-fontname))

(defun +emoji/register (range)
  "Use `+emoji/fontname' to cover unicode codepoint RANGE."
  (+emoji/register-font-for-range +emoji/fontname range))

(defun +emoji/lets-effing-go nil
  "Enable the emoji."
  (interactive)
  (+emoji/register-fallback)
  (mapc '+emoji/register +emoji/ranges))

(when (display-graphic-p)
  (+emoji/lets-effing-go))
;; Unicode fonts 🮲🮳:3 ends here
(provide '+emoji)
