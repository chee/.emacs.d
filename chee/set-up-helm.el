;; helm config

;; [[file:../../notebook/docfiles/emacs/setup/helm.org::*helm config][helm config:1]]
(use-package helm
		:elpaca t
		:defer 0.1
		:bind
		("M-x" . 'helm-M-x)
		("C-x C-f" . #'helm-find-files)
		:config
		;; (use-package helm-frame
			 ;; :elpaca t
			 ;; :config (add-hook 'helm-after-action-hook 'helm-frame-delete)
			 ;; (add-hook 'helm-cleanup-hook 'helm-frame-delete)
			 ;; (setq helm-split-window-preferred-function 'helm-frame-window))
		(use-package helm-projectile
			 :elpaca t
			 :after projectile
			 :config (helm-projectile-on))
		(use-package helm-company :elpaca t :after company)
		(use-package helm-lsp :elpaca t :after lsp)
		(use-package helm-org :elpaca t :after org)
		(use-package helm-swoop :elpaca t)
		(use-package helm-tramp :elpaca t)
		(use-package helm-slime :elpaca t :after slime)
		(use-package ripgrep :elpaca t)
		(use-package helm-rg :elpaca t :after ripgrep projectile)
		(use-package helm-ag :elpaca t :after projectile)
		(use-package helm-config :elpaca nil)
		(helm-mode 1))

(provide 'set-up-helm)
;; helm config:1 ends here
