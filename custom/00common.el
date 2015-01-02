;;; 00common.el --- Common settings.
;;
;;; Commentary:
;;
;;; Code:

(require 'use-package)

(setq auto-save-default nil)
(setq make-backup-files nil)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "conkeror")

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Initial major mode is Emacs Lisp mode
(setq initial-major-mode 'emacs-lisp-mode)

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Bindings

;; Backward kill word
(bind-key "C-w"      'backward-kill-word)
(bind-key "C-x C-k"  'kill-region)
(bind-key "\C-c C-k" 'kill-region)
(bind-key "C-x \\"   'align-current)

;; Lisp & Slime

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; defadvices

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Never kill scratch buffer."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; auto saving
;;https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org#saving 

(defun auto-save-command ()
  (let* ((basic (and buffer-file-name
                     (buffer-modified-p (current-buffer))
                     (file-writable-p buffer-file-name)
                     (not org-src-mode)))
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

(advise-commands "auto-save"
                 (ido-switch-buffer ace-window magit-status windmove-up windmove-down windmove-left windmove-right)
                 before
                 (auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'auto-save-command)
(add-hook 'focus-out-hook 'auto-save-command)

;; Packages

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package dired
  :config
  (progn
    (setq dired-recursive-copies 'always
          dired-recursive-deletes 'always)))

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package projectile
  :defer t
  :init (projectile-global-mode))

(use-package flycheck
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode))
  :init
  (progn
    (use-package flycheck-package)))

(use-package yasnippet
  :init
  (progn
    (let ((snippets-dir (expand-file-name "snippets" user-emacs-directory)))
      (yas-load-directory snippets-dir)
      (setq yas-snippets-dir snippets-dir))
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package google-translate
  :init
  (progn
    (setq google-translate-default-target-language "es")
    (setq google-translate-default-source-language "en"))
  :bind (("C-c t" . google-translate-at-point)
	 ("C-c T" . google-translate-query-translate)
	 ("C-c r" . google-translate-at-point-reverse)
	 ("C-c R" . google-translate-query-translate-reverse)))

(use-package restclient)

(use-package helm-dash
  :config
  (progn
    (setq helm-dash-min-length 1)
    (setq helm-dash-docsets-path (expand-file-name "dash-docsets" user-emacs-directory))
    (setq helm-dash-common-docsets '("Redis" "Go" "Emacs Lisp" "Common Lisp" "Clojure"))
    (setq helm-dash-browser-func 'eww)))

(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package multiple-cursors
  :config
  (progn
    (add-to-list 'mc/unsupported-minor-modes 'smartparens-mode))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package smartparens-config
  :init
  (progn
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)))

(use-package ag
  :config
  (progn
    (setq ag-highlight-search t)))

(use-package wgrep)
(use-package wgrep-ag)

(use-package eww
  :init
  (use-package eww-lnum
    :ensure t
    :init
    (progn (define-key eww-mode-map "f" 'eww-lnum-follow)
	      (define-key eww-mode-map "F" 'eww-lnum-universal)))
  (progn
    (add-hook 'eww-mode-hook
	      (lambda()
		(setq show-trailing-whitespace nil)))))

(use-package flyspell
  :init
  (progn
    (setq ispell-program-name "aspell" ; use aspell instead of ispell
	  ispell-extra-args '("--sug-mode=ultra"))
    (setq ispell-list-command "--list")
    (setq flyspell-issue-message-flag)
    (add-hook 'text-mode-hook 'flyspell-mode 1)))

;; company-mode
(use-package company
  :init (global-company-mode)
  :bind ("M-<tab>" . company-complete))

(use-package paradox
  :ensure t
  :config
  (setq paradox-execute-asynchronously t))

(defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "spanish") "english" "spanish")))
        (ispell-change-dictionary change)
        (message "Dictionary switched from %s to %s" dic change)
        ))
      (global-set-key (kbd "<f8>")   'fd-switch-dictionary)

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

(provide '00common)
;;; 00common.el ends here
