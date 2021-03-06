;;; init.el --- Initial configuration file.
;;
;;; Commentary:
;; - use-package: Load packages and configure them.
;; - emacs server: yay!
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

(use-package server
  :config (server-start))

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; Add ~/.emacs.d/custom/ to load-path
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load (expand-file-name "secrets.el" user-emacs-directory))
(load "00common.el")
(load "01lookandfeel.el")
(load "02chat.el")
(load "03news-email.el")
(load "04ruby.el")
(load "05clojure.el")
(load "06org.el")
(load "07elisp.el")
(load "08vc.el")
(load "09browsers.el")
(load "10scala.el")
(load "11golang.el")

(provide 'init)
;;; init.el ends here
