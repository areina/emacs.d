;;; 00common.el --- Common settings.
;;
;;; Commentary:
;;
;;; Code:

(require 'use-package)

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq history-length 1000)

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Bindings

(bind-keys
 :map global-map
 ;; Font size
 ("C-+" . text-scale-increase)
 ("C--" . text-scale-decrease)
 ;; kill bindings
 ("C-w" . backward-kill-word)
 ("C-x M-k" . kill-current-buffer)
 ("C-x C-k" . kill-region)
 ("C-x \\" . align-current)
 ;;http://endlessparentheses.com/Meta-Binds-Part-1%253A-Drunk-in-the-Dark.html
 ("M-9" . backward-sexp)
 ("M-0" . forward-sexp)
 ("M-1" . delete-other-windows))

(setq tab-always-indent 'complete)

;; Packages

;; Lisp & Slime

(use-package slime
  :disabled t
  :config
  (progn
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl")))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package dired
  :config
  (progn
    (setq dired-listing-switches "-lGaht --group-directories-first"
	  dired-recursive-copies 'always
	  dired-recursive-deletes 'always
	  dired-dwim-target t)))

(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (use-package ibuffer-vc :ensure t)

    (setq ibuffer-expert t
	  ibuffer-show-empty-filter-groups nil
	  ibuffer-filter-group-name-face 'font-lock-doc-face)

    (defvar toni/ibuffer-filter-groups
      '(("Emacs" (or
		  (mode . help-mode)
		  (mode . Info-mode)
		  (mode . apropos-mode)
		  (mode . fundamental-mode)
		  (mode . inferior-emacs-lisp-mode)
		  (name . "^\\*scratch\\*$")
		  (name . "^\\*Messages\\*$")))
	("Org" (or
		(mode . org-mode)
		(mode . org-agenda-mode)))
	("Chat" (mode . erc-mode))
	("Email" (or
		  (mode . mu4e-headers-mode)
		  (mode . mu4e-view-mode)
		  (mode . mu4e-compose-mode)
		  (mode . mail-mode))))
      "My custom filter groups for ibuffer")

    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))

    (setq ibuffer-formats
	  '((mark modified read-only vc-status-mini " "
		  (name 18 18 :left :elide)
		  " "
		  (size-h 9 -1 :right)
		  " "
		  (mode 16 16 :left :elide)
		  " "
		  filename-and-process)
	    (mark modified read-only vc-status-mini " "
		  (name 18 18 :left :elide)
		  " "
		  (size-h 9 -1 :right)
		  " "
		  (mode 16 16 :left :elide)
		  " "
		  (vc-status 16 16 :left)
		  " "
		  filename-and-process)))

    (defun toni/ibuffer-hook ()
      (interactive)
      (setq ibuffer-filter-groups
	    (append toni/ibuffer-filter-groups
		    (ibuffer-vc-generate-filter-groups-by-vc-root)))
      (ibuffer-auto-mode +1)
      (ibuffer-do-sort-by-alphabetic))

    (add-hook 'ibuffer-hook 'toni/ibuffer-hook)))

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy)
  :init
  (progn
    (projectile-global-mode)
    (defun toni-android-reload ()
      (interactive)
      (projectile-with-default-dir (projectile-project-root)
	(async-shell-command "script/reload-android-app"))))
  :bind (("C-c ." . toni-android-reload)))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (progn
    (global-flycheck-mode)
    (use-package flycheck-package
      :ensure t)))

(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode t)
    (setq yas-verbosity 1
	  yas-prompt-functions '(yas-ido-prompt)
	  yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))))

(use-package vlf
  :ensure t)

(use-package sass-mode
  :ensure t
  :disabled t)

(use-package toml-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" . yaml-mode))

(use-package nix-mode
  :ensure t)

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-default-target-language "es"
	google-translate-default-source-language "en")
  :bind (("C-x t" . google-translate-at-point)
	 ("C-x T" . google-translate-query-translate)
	 ;; ("C-x r" . google-translate-at-point-reverse) ;; C-x r is for bookmarks
	 ("C-x R" . google-translate-query-translate-reverse)))

(use-package web-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package helm
  :ensure t)

(use-package helm-dash
  :ensure t
  :config
  (progn
    (setq helm-dash-min-length 1)
    (setq helm-dash-docsets-path (expand-file-name "dash-docsets" user-emacs-directory))
    (setq helm-dash-common-docsets '("Redis" "Go" "Emacs Lisp" "Common Lisp" "Clojure"))
    (setq helm-dash-browser-func 'eww)))

(use-package browse-kill-ring
  :ensure t
  :bind (("s-y" . browse-kill-ring)))

(use-package align
  :bind (("C-c A a" . align)
	 ("C-c A c" . align-current)
	 ("C-c A r" . align-regexp)))

(use-package multiple-cursors ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c m e" . mc/mark-more-like-this-extended)
	 ("C-c m h" . mc/mark-all-like-this-dwim)
	 ("C-c m l" . mc/edit-lines)
	 ("C-c m n" . mc/mark-next-like-this)
	 ("C-c m p" . mc/mark-previous-like-this)
	 ("C-c m r" . vr/mc-mark)
	 ("C-c m C-a" . mc/edit-beginnings-of-lines)
	 ("C-c m C-e" . mc/edit-ends-of-lines)
	 ("C-c m C-s" . mc/mark-all-in-region))
  :config
  (progn
    (setq mc/mode-line
	  ;; Simplify the MC mode line indicator
	  '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
			face font-lock-warning-face))
    (add-to-list 'mc/unsupported-minor-modes 'smartparens-mode)))

(use-package visual-regexp ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c r" . vr/query-replace)
	 ("C-c R" . vr/replace)))

(use-package delsel ; Delete the selection instead of insert
  :config
  (delete-selection-mode))

;; https://ebzzry.github.io/emacs-pairs.html
(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode 1))
  :init
  (progn
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))
  :bind (("C-M-a" . sp-beginning-of-sexp)
	 ("C-M-e" . sp-end-of-sexp)

	 ("C-<down>" . sp-down-sexp)
	 ("C-<up>"   . sp-up-sexp)
	 ("M-<down>" . sp-backward-down-sexp)
	 ("M-<up>"   . sp-backward-up-sexp)

	 ("C-M-f" . sp-forward-sexp)
	 ("C-M-b" . sp-backward-sexp)

	 ("C-M-n" . sp-next-sexp)
	 ("C-M-p" . sp-previous-sexp)

	 ("C-S-f" . sp-forward-symbol)
	 ("C-S-b" . sp-backward-symbol)

	 ("C-<right>" . sp-forward-slurp-sexp)
	 ("M-<right>" . sp-forward-barf-sexp)
	 ("C-<left>"  . sp-backward-slurp-sexp)
	 ("M-<left>"  . sp-backward-barf-sexp)

	 ("C-M-t" . sp-transpose-sexp)
	 ("C-M-k" . sp-kill-sexp)
	 ("C-k"   . sp-kill-hybrid-sexp)
	 ("M-k"   . sp-backward-kill-sexp)
	 ("C-M-w" . sp-copy-sexp)

	 ("C-M-d" . delete-sexp)

	 ("M-<backspace>" . backward-kill-word)
	 ("C-<backspace>" . sp-backward-kill-word)
	 ([remap sp-backward-kill-word] . backward-kill-word)

	 ("M-[" . sp-backward-unwrap-sexp)
	 ("M-]" . sp-unwrap-sexp)

	 ("C-x C-t" . sp-transpose-hybrid-sexp)

	 ("C-c ("  . wrap-with-parens)
	 ("C-c ["  . wrap-with-brackets)
	 ("C-c {"  . wrap-with-braces)
	 ("C-c '"  . wrap-with-single-quotes)
	 ("C-c \"" . wrap-with-double-quotes)
	 ("C-c _"  . wrap-with-underscores)
	 ("C-c `"  . wrap-with-back-quotes)))

(use-package eldoc
  :diminish eldoc-mode)

(use-package esup
  :ensure t)

(use-package ag
  :ensure t
  :config
  (progn
    (setq ag-highlight-search t)))

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package ispell
  :ensure t
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra")
	ispell-list-command "--list")
  :init
  (progn
    (defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
	     (change (if (string= dic "spanish") "english" "spanish")))
	(ispell-change-dictionary change)
	(message "Dictionary switched from %s to %s" dic change))))
  :bind (("<f8>" . fd-switch-dictionary)))

(use-package flyspell
  :diminish flyspell-mode
  :init
  (progn
    (dolist (hook '(text-mode-hook message-mode-hook))
      (add-hook hook 'turn-on-flyspell))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (setq flyspell-issue-message-flag nil))

(use-package company
  :ensure t
  :config
  (progn
    (global-company-mode)
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
	  ;; Easy navigation to candidates with M-<n>
	  company-show-numbers t))
  :diminish company-mode)

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

(use-package highlight-symbol
  :ensure t
  :bind (("M-n" . highlight-symbol-next)
	 ("M-p" . highlight-symbol-prev)))

(use-package paradox ; Better package menu
  :ensure t
  :config
  ;; Don't ask for a token, please
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t))

(use-package elisp-mode
  :config
  (progn
    ;; Initial major mode is Emacs Lisp mode
    (setq initial-major-mode 'emacs-lisp-mode)
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELISP")))))

(use-package puppet-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package find-func
  :bind (("C-x F" . find-function-at-point)
	 ("C-x V" . find-variable-at-point)
	 ("C-x K" . find-function-on-key)))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :config
  (progn
    (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :init
  (progn
    (ivy-mode 1)
    (use-package counsel
      :ensure t
      :bind (("M-x" . counsel-M-x)
	     ("C-x C-f" . counsel-find-file))
      :init
      (progn
	(define-key read-expression-map (kbd "C-r") 'counsel-expression-history))))
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume))
  :config
  (progn
    (setq ivy-use-virtual-buffers t)))

(use-package prodigy
  :ensure t)

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

(bind-key* "C-;" #'endless/comment-line)

(defun t-pull-request()
  (interactive)
  (async-shell-command "hub pull-request")
  (with-current-buffer "*Async Shell Command*"
    (add-hook 'comint-output-filter-functions
	      '(lambda (txt)
		 (if (string-match-p "https" txt)
		     (browse-url txt)))
	      nil t)))

(defun public-ip ()
  "Get your public IP."
  (interactive)
  (let ((ip (get-public-ip)))
    (kill-new ip)
    (message (format "Your IP: %s" ip))))

(defun get-public-ip()
  "Get your public IP using dig and opendns."
  (let ((cmd "dig +short myip.opendns.com @resolver1.opendns.com"))
    (replace-regexp-in-string "\n" "" (shell-command-to-string cmd))))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; auto saving
;;https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org#saving

(defun auto-save-command ()
  (let* ((basic (and buffer-file-name
		     (buffer-modified-p (current-buffer))
		     (file-writable-p buffer-file-name)
		     ;; (not org-src-mode)
		     ))
	 (proj (and (projectile-project-p)
		    basic)))
    (if proj
	(projectile-save-project-buffers)
      (when basic
	(save-buffer)))))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
		 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
		    ,@body))
	       commands)))

;; defadvices

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Never kill scratch buffer."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
	(bury-buffer)
      ad-do-it)))

(advise-commands "auto-save"
		 (ido-switch-buffer ace-window magit-status windmove-up windmove-down windmove-left windmove-right)
		 before
		 (auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'auto-save-command)
(add-hook 'focus-out-hook 'auto-save-command)

(provide '00common)
;;; 00common.el ends here
