;;; 05clojure.el --- Clojure stuff.
;;
;;; Commentary:
;;
;;; Code:

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
    (setq nrepl-hide-special-buffers t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-stacktrace-fill-column 80)
    (setq nrepl-buffer-name-show-port t)
    (setq cider-repl-result-prefix ";; => ")
    (setq cider-repl-history-size 1000)
    (setq cider-repl-history-file (expand-file-name ".cider_repl_history" user-emacs-directory))))

(use-package clj-refactor
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook (lambda ()
				   (clj-refactor-mode 1)
				   (cljr-add-keybindings-with-prefix "C-c C-m")))))

(provide '05clojure)
;;; 05clojure.el ends here
