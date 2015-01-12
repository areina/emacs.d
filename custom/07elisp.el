;;; 07elisp.el --- Elisp settings.
;;
;;; Commentary:
;;
;;; Code:

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package elisp-slime-nav
      :ensure t
      :defer t
      :diminish elisp-slime-nav-mode
      :config
      (progn
	(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
	(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)))
    (use-package ert
      :defer t
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :mode ("Cask" . emacs-lisp-mode))

(provide '07elisp)
;;; 07elisp.el ends here
