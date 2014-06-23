;;; 05clojure.el --- Clojure stuff.
;;
;;; Commentary:
;;
;;; Code:

(use-package cider
  :init
  (progn
    (use-package ac-nrepl))
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

(provide '05clojure)
;;; 05clojure.el ends here
