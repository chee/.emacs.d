;;; -*- lexical-binding: t -*-
;;; set-up-patchwork.el -- automerge documents and live pushwork folders
;;; Commentary:
;; patchwork-lsp's Emacs package, straight from the checkout.
;; - files in a folder with .pushwork/config.json sync live (lsp-mode add-on)
;; - M-x patchwork-open (or C-x C-f automerge:<id>) opens automerge URLs (eglot)
;; - M-x patchwork-mirror writes a folder to disk and syncs edits back
;; The server is `patchwork-lsp' on PATH (exec-path-from-shell brings in
;; ~/.volta/bin). Right now that is the checkout itself, linked with
;; `cd ~/soft/chee/patchwork-lsp && npm link', so `pnpm build' there is all a
;; server change needs; `npm install -g patchwork-lsp' swaps in the release.
;; To try another server.cjs without relinking, set PATCHWORK_LSP_SERVER or
;; `patchwork-server-command'.
;;; Code:
(provide 'set-up-patchwork)

(use-package patchwork
  :ensure nil
  :if (file-exists-p "~/soft/chee/patchwork-lsp/editors/emacs/patchwork.el")
  :load-path "~/soft/chee/patchwork-lsp/editors/emacs/"
  :custom
  ;; lsp-mode is the client everywhere else, so patchwork-lsp rides along as an
  ;; add-on server in pushwork folders. automerge: buffers always use eglot.
  (patchwork-pushwork-client 'lsp-mode)
  ;; nil: $PATCHWORK_LSP_SERVER, else patchwork-lsp on exec-path
  (patchwork-server-command nil)
  :config
  (patchwork-mode 1))
;;; set-up-patchwork.el ends here
