;;; 07elisp.el --- Elisp settings.
;;
;;; Commentary:
;;
;;; Code:

(use-package emacs-lisp-mode
  :mode ("Cask" . emacs-lisp-mode))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode))

(use-package ert
  :init (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))

(use-package macrostep
  :ensure t)

(provide '07elisp)
;;; 07elisp.el ends here
