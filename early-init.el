;; Early inhibitions


;; [[file:../notebook/docfiles/emacs/emacs.org::*Early inhibitions][Early inhibitions:1]]
;; THIS IS AUTOGENERATED, ANYTHING THAT CHANGES WILL BE DESTROYED
(setq package-enable-at-startup nil)
(setq inhibit-default-init t)
(setq load-no-native t)

;;; HACK Work around native compilation on macOS failing with 'ld: library not
;;; found for -lemutls_w'.
;;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554
(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/opt/gcc/lib/gcc/14"
	   "/opt/homebrew/opt/libgccjit/lib/gcc/14"
	   "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14")
	 ":"))
;; Early inhibitions:1 ends here
