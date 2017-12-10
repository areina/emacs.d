;;; 08vc.el --- Version Control settings.
;;
;;; Commentary:
;; - Magit settings mainly
;;
;;; Code:

(require 'use-package)

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t)

(use-package magithub
  :ensure t
  :after magit
  :config (magithub-feature-autoinject t))

(use-package magit
  :ensure t
  :init
  (progn
    (defun t-magit-branches-show-merged (origin-fun)
      "Call magit-refresh-branch-manager and add info about merged branches.
On magit branch manager, add [merged] in the end of each branch line if this one
 is already merged."
      (funcall origin-fun)
      (goto-char (point-min))
      (let ((merged-branches (mapcar (lambda (b) (s-trim b))
				     (magit-git-lines "branch" "--merged" "master"))))
	(while (progn
		 (let ((branch-at-point (progn
					  (search-forward "   ")
					  (thing-at-point 'symbol))))
		   (when (member branch-at-point merged-branches)
		     (end-of-line)
		     (insert "[merged]")))
		 (forward-line 1)
		 (not (looking-at "^$"))))))

    (advice-add 'magit-refresh-branch-manager :around #'t-magit-branches-show-merged))
  :bind ("C-x g" . magit-status))

(provide '08vc)
