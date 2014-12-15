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

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

(use-package solarized-theme
  :init
  (progn
    (load-theme 'solarized-dark t)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package golden-ratio
  :diminish " φ"
  :init
  (golden-ratio-mode 1))

(use-package git-gutter
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

(provide '01lookandfeel)
;;; 01lookandfeel.el ends here
