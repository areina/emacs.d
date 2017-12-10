;;; 01lookandfeel.el --- Prettify emacs.
;;
;;; Commentary:
;;
;;; Code:

;; use-package
(require 'use-package)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(use-package time
  :config (display-time)
  :init
  (setq display-time-day-and-date t
        display-time-24hr-format t))

(setq inhibit-startup-message t)

(when (display-graphic-p)
  (set-frame-font "Fira Mono 11"))

;; Remove selected region if typing
(pending-delete-mode 0)

;; y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package whitespace
  :init
  (progn
    (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                  empty trailing lines-tail)
          whitespace-line-column nil)
    (dolist (hook '(prog-mode-hook conf-mode-hook))
      (add-hook hook #'whitespace-mode)))
  :diminish whitespace-mode)

(setq use-dialog-box nil)

(setq ns-use-srgb-colorspace t)
;; Anti-aliasing
(setq mac-allow-anti-aliasing t)

(defvar laf-mood 'light)
;;(defvar laf-mood 'dark)

(if (eq laf-mood 'light)
    (use-package leuven-theme
      :ensure t
      :config
      (progn
        (load-theme 'leuven t)))
  (if (display-graphic-p)
      (use-package zerodark-theme
        :ensure zerodark-theme
        :config
        (progn (load-theme 'zerodark t)
               (zerodark-setup-modeline-format)))
    (load-theme 'wheatgrass 'no-confirm)))

(use-package all-the-icons
  :ensure t)

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package paren ; Highlight paired delimiters
  :config (show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
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
  :config (global-hi-lock-mode))

(use-package golden-ratio
  :defer t
  :ensure t
  :diminish " φ"
  :config
  (golden-ratio-mode 1)
  :init
  (setq golden-ratio-auto-scale t))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (progn
    (beacon-mode 1)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode)
  :init
  (progn
    (setq git-gutter:separator-sign " "
          git-gutter:lighter " GG"))
  :config
  (progn
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

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (progn
    (add-to-list 'page-break-lines-modes 'org-agenda-mode)
    (global-page-break-lines-mode t)))

(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(add-hook 'prog-mode-hook 'my/add-watchwords)

(provide '01lookandfeel)
;;; 01lookandfeel.el ends here
