;;; 05clojure.el --- Clojure stuff.
;;
;;; Commentary:
;;
;;; Code:

(use-package clojure-mode
  :ensure t
  :mode (("\\.edn$" . clojure-mode)
	 ("\\.cljc$" . clojure-mode))
  :config
  (progn
    (add-hook 'clojure-mode-hook (lambda () (setq mode-name "CLJ")))
    (add-hook 'clojurescript-mode-hook (lambda () (setq mode-name "CLJS")))
    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2)
      (let-routes 1))))

(use-package cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
    (setq nrepl-hide-special-buffers t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-stacktrace-fill-column 80)
    (setq nrepl-buffer-name-show-port t)
    (setq cider-repl-result-prefix ";; => ")
    (setq cider-repl-history-size 1000)
    (setq cider-refresh-before-fn "reloaded.repl/suspend")
    (setq cider-refresh-after-fn "reloaded.repl/resume")
    (setq cider-repl-history-file
	  (expand-file-name ".cider_repl_history" user-emacs-directory)))
  :init
  (progn
    (defun toni-clj-reset ()
      (interactive)
      (save-some-buffers)
      (cider-interactive-eval "(reloaded.repl/reset)")))
  :bind (("C-c SPC" . toni-clj-reset)))

(use-package clj-refactor
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook
	      (lambda ()
		(clj-refactor-mode 1)
		(cljr-add-keybindings-with-prefix "C-c C-m")))))

(provide '05clojure)
;;; 05clojure.el ends here
