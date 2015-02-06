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

    (advice-add 'magit-refresh-branch-manager :around #'t-magit-branches-show-merged)

    (use-package magit-gh-pulls
      :ensure t
      :config
      (setq magit-gh-pulls-collapse-commits t)
      :init
      (progn
	(with-eval-after-load 'magit
	  (define-key magit-mode-map "#gg" 'endless/load-gh-pulls-mode))))

    (defun endless/load-gh-pulls-mode ()
      "Start `magit-gh-pulls-mode' only after a manual request."
      (interactive)
      (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
      (magit-gh-pulls-mode 1)
      (magit-gh-pulls-reload)))
  :bind ("C-x g" . magit-status))

(provide '08vc)
