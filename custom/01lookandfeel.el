;;; 01lookandfeel.el --- Prettify emacs.
;;
;;; Commentary:
;;
;;; Code:

;; use-package
(require 'use-package)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Time
(setq display-time-day-and-date t display-time-24hr-format t)
(display-time)

;; Do not show startup message
(setq inhibit-startup-message t)

;; Default font
(add-to-list 'default-frame-alist
             '(font . "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
(set-frame-font "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; Remove selected region if typing
(pending-delete-mode 0)

;; y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
				empty trailing lines-tail)
	whitespace-line-column nil)
  :diminish whitespace-mode)

(use-package solarized
  :ensure solarized-theme
  :defer t
  :init (load-theme 'solarized-dark 'no-confirm))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package hl-line
  :init (global-hl-line-mode 1))

(use-package paren ; Highlight paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
		show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
	  (add-hook hook #'rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package hi-lock
  :defer t
  :init (global-hi-lock-mode))

(use-package golden-ratio
  :defer t
  :ensure t
  :diminish " φ"
  :init
  (golden-ratio-mode 1))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode)
  :config
  (progn
    (setq git-gutter:separator-sign " ")
    (setq git-gutter:lighter " GG")
    (set-face-background 'git-gutter:deleted "#990A1B")
    (set-face-foreground 'git-gutter:deleted "#990A1B")
    (set-face-background 'git-gutter:modified "#00736F")
    (set-face-foreground 'git-gutter:modified "#00736F")
    (set-face-background 'git-gutter:added "#546E00")
    (set-face-foreground 'git-gutter:added "#546E00"))
  :bind (("C-x p" . git-gutter:previous-hunk)
	 ("C-x n" . git-gutter:next-hunk)
	 ("C-x v =" . git-gutter:popup-hunk)
	 ("C-x v r" . git-gutter:revert-hunk)))

(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(add-hook 'prog-mode-hook 'my/add-watchwords)

(provide '01lookandfeel)
;;; 01lookandfeel.el ends here
