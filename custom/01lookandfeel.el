;;; 01lookandfeel.el --- Prettify emacs.
;;
;;; Commentary:
;;
;;; Code:

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

(provide '01lookandfeel)
;;; 01lookandfeel.el ends here
