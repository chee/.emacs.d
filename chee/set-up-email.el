;; email


;; [[file:../../notebook/docfiles/emacs/setup/email.org::*email][email:1]]
(provide 'set-up-email)
;; email:1 ends here

;; smtp


;; [[file:../../notebook/docfiles/emacs/setup/email.org::*smtp][smtp:1]]
(use-package smtpmail :ensure nil
	 :config
	 (setq smtpmail-smtp-server "fastmail"
	  smtpmail-local-domain nil
	  smtpmail-smtp-user "yay@chee.party"
	  smtpmail-mail-address "yay@chee.party"
	  smtpmail-servers-requiring-authorization "localhost"
	  smtpmail-smtp-service 465
	  smtpmail-stream-type 'ssl
	  send-mail-function 'smtpmail-send-it))
;; smtp:1 ends here

;; gnus


;; [[file:../../notebook/docfiles/emacs/setup/email.org::*gnus][gnus:1]]
(use-package gnus :ensure nil
	 :commands gnus
	 :config
	 ;; second system energy
	 ;; https://web.archive.org/web/19981206063754/http%3A//www.mozilla.org/blue-sky/misc/199805/intertwingle.html
	 (add-hook 'message-mode-hook #'flyspell-mode)
	 (setq gnus-select-method nil)
	 (setq epa-file-cache-passphrase-for-symmetric-encryption t)
	 (setq gnus-select-method
	  '(nnimap "fastmail"
			(nnimap-address "imap.fastmail.com")
			(nnimap-server-port 993)
			(nnimap-inbox "INBOX")
			(nnir-search-engine imap)
			;;(nnimap-split-methods default)
			(nnimap-expunge t)
			(nnimap-stream ssl)))

	 (setq gnus-use-cache t))
;; gnus:1 ends here
