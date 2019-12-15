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

												flycheck
												lsp-mode
												lsp-ui
												company
												company-lsp
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
												go-playground
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
(require 'lsp-mode)
(require 'go-mode)
(ido-mode t)
(editorconfig-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(go-staticcheck go-vet))

;; auto-encryption for *.gpg files
(require 'epa-file)

;; autocomplete
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)
(setq company-lsp-async t)
(setq lsp-enable-snippet nil)

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
  (lsp)
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
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
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

;; python mode hooks
(require 'jedi)
(defun python-mode-setup ()
  "Setups the Python development environment."
  (lsp)
  )
(add-hook 'python-mode-hook 'python-mode-setup)

;; whitespace cleaning
(require 'whitespace)
(setq whitespace-style (quote
												 ( face trailing )))
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-whitespace-toggle-options t)

(provide 'init)
;;; init.el ends here
