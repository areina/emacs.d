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
      (testing 0)
      (let-routes 1))))

(use-package cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))
  :init
  (progn
    (setq nrepl-hide-special-buffers t
          cider-repl-pop-to-buffer-on-connect nil
          cider-stacktrace-fill-column 80
          nrepl-buffer-name-show-port t
          cider-repl-result-prefix ";; => "
          cider-repl-history-size 1000
          cider-refresh-before-fn "reloaded.repl/suspend"
          cider-refresh-after-fn "reloaded.repl/resume"
          cider-repl-history-file
	  (expand-file-name ".cider_repl_history" user-emacs-directory))
    (defun toni-clj-reset ()
      (interactive)
      (save-some-buffers)
      (cider-interactive-eval "(reloaded.repl/reset)")))
  :bind (("C-c SPC" . toni-clj-reset)))

(use-package clj-refactor
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(provide '05clojure)
;;; 05clojure.el ends here
