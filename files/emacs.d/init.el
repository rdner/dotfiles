;;; init.el --- pragmader's emacs config file

;;; Commentary:
;; This configuration includes development environments:
;; Web: HTML/CSS
;; JavaScript
;; Go
;; Python

;;; Code:

;; packages
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
  '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
		 ("MELPA Stable" . "https://stable.melpa.org/packages/")
		 ("MELPA" . "https://melpa.org/packages/"))
  package-archive-priorities
  '(("MELPA Stable" . 10)
		 ("MELPA" . 5)
		 ("GNU ELPA" . 0)))
(package-initialize)
(defvar package-list '(
												gnu-elpa-keyring-update

												;; language servers
												lsp-mode
												lsp-ui
												company
												company-lsp
												flycheck
												flycheck-inline

												whitespace
												monokai-theme
												editorconfig
												magit

												;; general modes
												json-mode
												yaml-mode
												dockerfile-mode
												markdown-mode
												typescript-mode

												;; golang
												go-mode
												go-rename
												go-playground
												protobuf-mode

												;; rust
												rust-mode
												))


;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
(package-initialize) ; init autoloaded packages

;; appearance
(menu-bar-mode -1)
(global-hl-line-mode 1)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode t)
(load-theme 'monokai t)

;; key bindings
(global-set-key (kbd "C-c f") 'find-name-dired)
(global-set-key (kbd "C-c s") 'find-grep-dired)
(global-set-key (kbd "C-c C-s") 'find-grep)
(global-set-key (kbd "C-c C-g") 'magit-status)
(require 'xref)
(global-set-key (kbd "C-c .") #'(lambda () (interactive)
																	(xref-push-marker-stack)
																	(message "Pushed %s:%d to the marker stack"
																		(buffer-name)
																		(line-number-at-pos)
																		)
																	))

;; modes
(require 'lsp-mode)
(require 'go-mode)
(require 'rust-mode)
(ido-mode t)
(editorconfig-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(go-staticcheck go-vet))

;; auto-encryption for *.gpg files
(require 'epa-file)

;; autocomplete
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-q") 'company-complete)

(require 'company-lsp)
(push 'company-lsp company-backends)
(setq company-lsp-async t)
(setq lsp-enable-snippet nil)

;; lsp hooks
(add-hook 'go-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook `bash-mode-hook #'lsp)
(add-hook `dockerfile-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook `css-mode-hook #'lsp)
(add-hook `html-mode-hook #'lsp)
(add-hook `typescript-mode-hook #'lsp)
(add-hook `json-mode-hook #'lsp)
(add-hook `yaml-mode-hook #'lsp)

;; hooks
(add-hook 'prog-mode-hook #'hs-minor-mode) ; code block hide/show

;; go mode hooks
(defun go-mode-setup ()
  "Setups the Go development environment."
	(setenv "GO111MODULE" "on")
  (defun display-go-coverage ()
    "Displays coverage information for the current buffer in Go mode."
    (interactive)
    (shell-command "go test -coverprofile cover.out")
    (go-coverage "cover.out")
    (shell-command "rm cover.out")
    )

  (setq go-coverage-display-buffer-func 'display-buffer-same-window)
  (setq gofmt-command "goimports")
  (setq compile-command "go build -v")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (local-set-key (kbd "C-c c") 'display-go-coverage)
  (add-hook 'before-save-hook 'gofmt-before-save)
  )
(add-hook 'go-mode-hook 'go-mode-setup)

(defun rust-mode-setup ()
  "Setups the Rust development environment."
	(setq indent-tabs-mode nil)
	(setq rust-format-on-save t)
  (define-key (current-local-map) "\C-c\C-c" 'rust-run-clippy)
	)
(add-hook 'rust-mode-hook 'rust-mode-setup)

;; whitespace cleaning
(require 'whitespace)
(setq whitespace-style (quote
												 ( face trailing )))
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-whitespace-toggle-options t)

(provide 'init)
;;; init.el ends here
(custom-set-variables
	;; custom-set-variables was added by Custom.
	;; If you edit it by hand, you could mess it up, so be careful.
	;; Your init file should contain only one such instance.
	;; If there is more than one, they won't work right.
	'(package-selected-packages
		 (quote
			 (yaml-mode typescript-mode protobuf-mode monokai-theme magit lsp-ui json-mode go-rename go-playground gnu-elpa-keyring-update flycheck editorconfig dockerfile-mode company-lsp badwolf-theme))))
(custom-set-faces
	;; custom-set-faces was added by Custom.
	;; If you edit it by hand, you could mess it up, so be careful.
	;; Your init file should contain only one such instance.
	;; If there is more than one, they won't work right.
	)
