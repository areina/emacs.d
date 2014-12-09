;;; 08vc.el --- Version Control settings.
;;
;;; Commentary:
;; - Magit settings mainly
;;
;;; Code:

(require 'use-package)

(use-package magit
  :bind ("C-x g" . magit-status))

(eval-after-load 'magit
  '(define-key magit-mode-map "#gg"
     'endless/load-gh-pulls-mode))

(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

(provide '08vc)
