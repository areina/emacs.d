;;; 06org.el --- Org-mode settings.
;;
;;; Commentary:
;;
;; Nothing interesting here...
;;
;;; Code:

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(load-file (expand-file-name "vendor/org-protocol-github-lines.el" user-emacs-directory))

(setq org-protocol-github-project-directories '("/tmp/"))

(provide '06org)
;;; 06org.el ends here
