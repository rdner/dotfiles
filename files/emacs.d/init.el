;;; init.el --- pragmader's emacs config file

;;; Commentary:
;; This configuration includes development environments:
;; Web: HTML/CSS
;; JavaScript
;; Go

;;; Code:

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar package-list '(
											 flycheck
											 auto-complete
											 whitespace
											 badwolf-theme
											 editorconfig
											 magit

											 ;; general modes
											 yaml-mode
											 markdown-mode

											 ;; web
											 web-mode
											 tern
											 tern-auto-complete

											 ;; golang
											 go-mode
											 go-rename
											 go-autocomplete
											 go-playground
											 go-eldoc
											 protobuf-mode
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
(load-theme 'badwolf t)

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
(require 'go-mode)
(ido-mode t)
(editorconfig-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(go-staticcheck))

;; auto-encryption for *.gpg files
(require 'epa-file)

;; autocomplete
(require 'go-autocomplete) ; must be before the lines below
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)

;; hooks
(add-hook 'prog-mode-hook #'hs-minor-mode) ; code block hide/show

;; web mode hooks
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.(go)?html?\\'" . web-mode))

;; javascript
(defun js-mode-setup ()
	"Setups the JavaScript development environment."
	(tern-mode)
	)
(add-hook 'js-mode-hook 'js-mode-setup)

;; go mode hooks
(defun go-mode-setup ()
	"Setups the Go development environment."
	(defun display-go-coverage ()
		"Displays coverage information for the current buffer in Go mode."
		(interactive)
		(shell-command "go test -coverprofile cover.out")
		(go-coverage "cover.out")
		(shell-command "rm cover.out")
		)

	(setq go-coverage-display-buffer-func 'display-buffer-same-window)
	(setq compile-command "go build -v")
	(define-key (current-local-map) "\C-c\C-c" 'compile)
	(require 'go-eldoc)
	(go-eldoc-setup)
	(setq gofmt-command "goimports")
	(setq go-autocomplete-externals nil)
	(add-hook 'before-save-hook 'gofmt-before-save)
	(local-set-key (kbd "C-c r") 'go-rename)
	(local-set-key (kbd "C-c c") 'display-go-coverage)
	(local-set-key (kbd "C-c h") 'godoc-at-point)
	(local-set-key (kbd "M-.") 'godef-jump)
	(local-set-key (kbd "M-*") 'pop-tag-mark)
	)
(add-hook 'go-mode-hook 'go-mode-setup)
(setenv "GO111MODULE" "on")

(defun go-playground-mode-setup ()
	"Setups the Go development environment."
	(local-set-key (kbd "M-RET") 'go-playground-exec)
	(local-set-key (kbd "C-c r") 'go-playground-rm)
	)
(add-hook 'go-playground-mode-hook 'go-playground-mode-setup)

;; whitespace cleaning
(require 'whitespace)
(setq whitespace-style (quote
												( face trailing )))
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-whitespace-toggle-options t)

(provide 'init)
;;; init.el ends here
