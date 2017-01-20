;;; 06org.el --- Org-mode settings.
;;
;;; Commentary:
;;
;; Nothing interesting here...
;;
;;; Code:

(use-package org
  :defer t
  :bind (("C-c l" . org-store-link)
	 ("C-c b" . org-switchb))
  :config
  (progn
    (setq org-directory "~/.emacs.d/org"
	  org-default-notes-file "~/.emacs.d/org/refile.org"
	  org-agenda-files (quote ("~/.emacs.d/org/todo.org"
				   "~/.emacs.d/org/refmob.org"
				   "~/.emacs.d/org/refile.org"))
	  org-todo-keywords
	  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))))

(use-package org-bullets
  :ensure t
  :defer t
  :config
  (progn
    ;; org-bullets-bullet-list
    ;; default: "◉ ○ ✸ ✿"
    ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;; Small: ► • ★ ▸
    ;; others: ▼, ⤵, ↴, ⬎, ⤷,…, and ⋱.
    (setq org-bullets-bullet-list '("◆")
	  org-ellipsis " ⤵")
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org-capture
  :defer t
  :bind (("C-c c" . org-capture))
  :config
  (setq org-capture-templates
	(quote (("t" "todo" entry (file "~/.emacs.d/org/refile.org")
		 "* TODO %?\nSCHEDULED: %t\n")
		("n" "note" entry (file "~/.emacs.d/org/refile.org")
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("w" "org-protocol" entry (file "~/.emacs.d/org/refile.org")
		 "* TODO Review %c\n%U\n" :immediate-finish t)))))

(use-package org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-agenda-files (quote ("~/.emacs.d/org"))
	;; Overwrite the current window with the agenda
	org-agenda-window-setup 'current-window))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))

(provide '06org)
;;; 06org.el ends here
