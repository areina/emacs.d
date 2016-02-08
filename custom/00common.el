;;; 00common.el --- Common settings.
;;
;;; Commentary:
;;
;;; Code:

(require 'use-package)

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq history-length 1000)

;; Set default browser

(use-package browse-url
  :config
  (setq browse-url-generic-program (executable-find "conkeror")
	browse-url-browser-function '(("github" . browse-url-default-browser)
				      ("docs.google.com" . browse-url-chromium)
				      ("." . eww-browse-url))))
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

;;http://endlessparentheses.com/Meta-Binds-Part-1%253A-Drunk-in-the-Dark.html
(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)

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

(advise-commands "auto-save"
                 (ido-switch-buffer ace-window magit-status windmove-up windmove-down windmove-left windmove-right)
                 before
                 (auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'auto-save-command)
(add-hook 'focus-out-hook 'auto-save-command)

;; Packages

;; Lisp & Slime

(use-package slime
  :disabled t
  :defer 5
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
  (progn
    (projectile-global-mode)))

(use-package flycheck
  :ensure t
  :init
  (progn
    (global-flycheck-mode)
    (use-package flycheck-package
      :ensure t)))

(use-package yasnippet
  :ensure t
  :defer 5
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

(use-package smex
  :ensure t
  :init (smex-initialize)
  :config
  (progn
    (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))
  :bind ("M-x" . smex))

(use-package google-translate
  :ensure t
  :init
  (progn
    (setq google-translate-default-target-language "es")
    (setq google-translate-default-source-language "en"))
  :bind (("C-x t" . google-translate-at-point)
	 ("C-x T" . google-translate-query-translate)
	 ("C-x r" . google-translate-at-point-reverse)
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
  :bind (("C-c y" . browse-kill-ring)))

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

(use-package smartparens
  :ensure t
  :commands (smartparens-mode show-smartparens-mode)
  :config (require 'smartparens-config))

;; (use-package smartparens-config
;;   :ensure t
;;   :init
;;   (progn
;;     (setq sp-ignore-modes-list (append sp-ignore-modes-list '(magit-log-mode magit-key-mode)))
;;     (smartparens-global-mode 1)
;;     (show-smartparens-global-mode 1)
;;     (define-key sp-keymap (kbd "M-s") 'sp-splice-sexp)
;;     (define-key sp-keymap (kbd "C-<right>") #'sp-forward-slurp-sexp)))

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

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package jenkins
  :ensure t
  :config
  (progn
    (setq jenkins-api-token private-jenkins-token)
    (setq jenkins-hostname private-jenkins-url)
    (setq jenkins-username private-jenkins-username)))

(use-package eww
  :init
  (progn
    (defun oww-down (arg)
      (interactive "p")
      (if (bolp)
	  (progn
	    (forward-paragraph arg)
	    (forward-line 1))
	(line-move arg)))

    (defun oww-up (arg)
      (interactive "p")
      (if (bolp)
	  (progn
	    (forward-line -1)
	    (backward-paragraph arg)
	    (forward-line 1))
	(line-move (- arg))))

    (defun oww-mode-hook ()
      (define-key eww-mode-map "o" 'eww)
      (define-key eww-mode-map "n" 'oww-down)
      (define-key eww-mode-map "N" 'eww-next-url)
      (define-key eww-mode-map "p" 'oww-up)
      (define-key eww-mode-map "P" 'eww-previous-url)
      (define-key eww-mode-map "v" 'recenter-top-bottom)
      (define-key eww-mode-map "V" 'eww-view-source))

    (add-hook 'eww-mode-hook 'oww-mode-hook)
    (use-package eww-lnum
      :ensure t
      :defer t
      :init
      (progn
	(with-eval-after-load 'eww
	  (define-key eww-mode-map "f" (lambda() (interactive) (eww-lnum-follow 4)))
	  (define-key eww-mode-map "F" 'eww-lnum-universal)))))
  :config
  (setq shr-external-browser 'browse-url-generic))

(use-package ispell
  :config
  (progn
    (setq ispell-program-name "aspell" ; use aspell instead of ispell
	  ispell-extra-args '("--sug-mode=ultra")
	  ispell-list-command "--list")))

(use-package flyspell
  :init
  (progn
    (dolist (hook '(text-mode-hook message-mode-hook))
      (add-hook hook 'turn-on-flyspell))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-issue-message-flag nil)))

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

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume))
  :config
  (progn
    (setq ivy-use-virtual-buffers t)))

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

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "spanish") "english" "spanish")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(bind-key "<f8>" #'fd-switch-dictionary)

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
