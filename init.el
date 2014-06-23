;;; init.el --- Initial configuration file.
;;
;;; Commentary:
;; - Cask: Manage package dependencies
;; - Pallet: Mantain your caskfile synced with package-install.
;; - use-package: Load packages and configure them.
;; - emacs server: yay!
;;
;;; Code:


;; Package manager
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; use-package
(require 'use-package)

;; Add ~/.emacs.d/custom/ to load-path
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

(load (expand-file-name "secrets.el" user-emacs-directory))
(load "00common.el")
(load "01lookandfeel.el")
(load "02chat.el")
(load "03email.el")
(load "04ruby.el")
(load "05clojure.el")
(load "06org.el")
(load "07elisp.el")

(provide 'init)
;;; init.el ends here
