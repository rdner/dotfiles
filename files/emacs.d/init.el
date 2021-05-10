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
												exec-path-from-shell

												;; essentials
												flycheck
												flycheck-inline
												company
												xclip

												;; language server
												lsp-mode
												lsp-ui

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

;; fix PATH variable
(when (daemonp)
  (exec-path-from-shell-initialize))

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
(global-set-key (kbd "C-c -") 'hs-toggle-hiding)

;; this allows to leave code markers and go back to them via the stack
(require 'xref)
(global-set-key (kbd "C-c .") #'(lambda () (interactive)
																	(xref-push-marker-stack)
																	(message "Pushed %s:%d to the marker stack"
																		(buffer-name)
																		(line-number-at-pos)
																		)
																	))

;; modes
(ido-mode t)
(editorconfig-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(go-staticcheck go-vet))

;; auto-encryption for *.gpg files
(require 'epa-file)

;; autocomplete
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-q") 'company-complete)

(require 'company-capf)
(push 'company-capf company-backends)

;; hooks
(add-hook 'prog-mode-hook #'hs-minor-mode) ; code block hide/show

;; lsp-mode
(require 'lsp-mode)
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

(setq lsp-enable-snippet nil)
(setq lsp-enable-links nil)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-response-timeout 5)

(require 'lsp-ui)
(set-face-background 'lsp-ui-doc-background "black")
(set-face-background 'lsp-ui-peek-peek "black")

(defun lsp-mode-setup ()
	"Setups the lsp mode."

	;; disable annoying popups
	(setq lsp-ui-sideline-enable nil)
	(setq lsp-ui-doc-enable nil)
	(setq compilation-read-command nil)

	(local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
	(local-set-key (kbd "M-?") 'lsp-ui-peek-find-references)
	(local-set-key (kbd "C-c C-c") 'compile)
	(local-set-key (kbd "C-c C-e") 'lsp-ui-flycheck-list)
	(local-set-key (kbd "C-c C-d") 'lsp-ui-doc-glance)
	(local-set-key (kbd "C-c C-i") 'lsp-ui-peek-find-implementation)
	(local-set-key (kbd "C-c C-r") 'lsp-rename)

  (add-hook 'before-save-hook 'lsp-format-buffer)
  (add-hook 'before-save-hook 'lsp-organize-imports)
	)
(add-hook 'lsp-mode-hook 'lsp-mode-setup)

;; go mode
(require 'go-mode)
(defun display-go-coverage ()
	"Displays coverage information for the current buffer in Go mode."
	(interactive)
	(shell-command "go test -coverprofile cover.out")
	(go-coverage "cover.out")
	(shell-command "rm cover.out")
	)
(defun go-mode-setup ()
  "Setups the Go development environment."
	(setenv "GO111MODULE" "on")
  (setq go-coverage-display-buffer-func 'display-buffer-same-window)
  (setq compile-command "go build -v")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (local-set-key (kbd "C-c c") 'display-go-coverage)
  )
(add-hook 'go-mode-hook 'go-mode-setup)

;; rust mode
(require 'rust-mode)
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
	 '(rust-mode protobuf-mode go-playground go-mode typescript-mode dockerfile-mode yaml-mode json-mode magit editorconfig monokai-theme lsp-ui lsp-mode xclip company flycheck-inline flycheck gnu-elpa-keyring-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
