;;; init.el --- Initial configuration file.
;;
;;; Commentary:
;; - Cask: Manage package dependencies
;; - Pallet: Mantain your caskfile synced with package-install.
;; - use-package: Load packages and configure them.
;; - emacs server: yay!
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eww-lnum ensime zerodark-theme yard-mode yaml-mode wgrep-ag web-mode vlf visual-regexp use-package toml-mode smartparens slim-mode ruby-tools ruby-hash-syntax rspec-mode robe rhtml-mode restclient rainbow-mode rainbow-delimiters puppet-mode projectile prodigy paradox org-bullets nix-mode markdown-mode lua-mode jenkins jabber iedit hungry-delete highlight-symbol highlight-indentation helm-dash google-translate golden-ratio go-mode git-timemachine git-link git-gutter gist flycheck-package feature-mode expand-region esup erc-image erc-hl-nicks elisp-slime-nav elfeed-org dockerfile-mode counsel company clj-refactor browse-kill-ring beacon aurel aggressive-indent ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
