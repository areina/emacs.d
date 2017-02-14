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
	  org-todo-keywords
	  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
	  org-refile-targets (quote ((nil :maxlevel . 9)
				     (org-agenda-files :maxlevel . 9)))
	  org-refile-use-outline-path t
	  org-outline-path-complete-in-steps nil
	  ;; Allow refile to create parent tasks with confirmation
	  org-refile-allow-creating-parent-nodes 'confirm
	  org-deadline-warning-days 10
          org-log-done 'time
          org-log-redeadline 'time
          org-log-reschedule 'time
          org-agenda-block-separator "")))

(use-package org-bullets
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  (progn
    ;; org-bullets-bullet-list
    ;; default: "◉ ○ ✸ ✿"
    ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;; Small: ► • ★ ▸
    ;; others: ▼, ⤵, ↴, ⬎, ⤷,…, and ⋱.
    (setq org-bullets-bullet-list '("◆")
	  org-ellipsis " ⤵")))

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
	org-agenda-window-setup 'current-window
	org-agenda-span 'week))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))

(use-package org-plus-contrib
  :ensure t
  :config
  (progn
    (setq org-contacts-files (expand-file-name "contacts.org" org-directory)
          org-contacts-icon-use-gravatar nil)))

(provide '06org)
;;; 06org.el ends here


;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("h" "Habits" agenda "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	(" " "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					    (org-agenda-span 1)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-start-day "+0d")
					    (org-agenda-todo-ignore-deadlines nil)))
				(tags "REFILE-ARCHIVE-REFILE=\"nil\""
				      ((org-agenda-overriding-header "Tasks to Refile:")
				       (org-tags-match-list-sublevels nil)))
				(tags-todo "-CANCELLED-ARCHIVE/!NEXT"
					   ((org-agenda-overriding-header "Next Tasks:")))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
					   ((org-agenda-overriding-header "Active Projects:")
					    (org-agenda-skip-function 'gs/select-projects)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
					   ((org-agenda-overriding-header "Standalone Tasks:")
					    (org-agenda-skip-function 'gs/select-standalone-tasks)))
				(agenda "" ((org-agenda-overriding-header "Week At A Glance:")
					    (org-agenda-span 5)
					    (org-agenda-start-day "+1d")
					    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %s [%b] ")))))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
					   ((org-agenda-overriding-header "Remaining Project Tasks:")
					    (org-agenda-skip-function 'gs/select-project-tasks)))
				(tags "INACTIVE-ARCHIVE"
				      ((org-agenda-overriding-header "Inactive Projects and Tasks")
				       (org-tags-match-list-sublevels nil)))
				(tags "ENDOFAGENDA"
				      ((org-agenda-overriding-header "End of Agenda")
				       (org-tags-match-list-sublevels nil))))
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(clock))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (search . "  %i %-12:c")))
	  (org-agenda-todo-ignore-deadlines 'near)
	  (org-agenda-todo-ignore-scheduled t)))
	("X" "Agenda" ((agenda "") (alltodo))
	 ((org-agenda-ndays 10)
	  (org-agenda-start-on-weekday nil)
	  (org-agenda-start-day "-1d")
	  (org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state)))
	 )))

(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
	(stuck (gs/org-agenda-project-warning))) ; warning for stuck projects
    (if (> (length path) 0)
	(concat stuck ; add stuck warning
		" [" path "]") ; add "breadcrumb"
      stuck)))

(defun gs/org-agenda-project-warning ()
  "Is a project stuck or waiting. If the project is not stuck,
show nothing. However, if it is stuck and waiting on something,
show this warning instead."
  (if (gs/org-agenda-project-is-stuck)
      (if (gs/org-agenda-project-is-waiting) " !W" " !S") ""))

(defun gs/org-agenda-project-is-waiting ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
	(save-excursion
	  (re-search-forward "^\\*+ WAITING" subtree-end t)))
    nil)) ; if it's not a project, return an empty string

(defun gs/org-agenda-project-is-stuck ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	     (has-next))
	(save-excursion
	  (forward-line 1)
	  (while (and (not has-next)
		      (< (point) subtree-end)
		      (re-search-forward "^\\*+ NEXT " subtree-end t))
	    (unless (member "WAITING" (org-get-tags-at))
	      (setq has-next t))))
	(if has-next nil t)) ; signify that this project is stuck
    nil)) ; if it's not a project, return an empty string

;; == bh/helper-functions ==
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))
(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))



(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
	(concat "{" loc "} ")
      "")))

;; Variables for ignoring tasks with deadlines
(defvar gs/hide-deadline-next-tasks t)

(setq org-agenda-tags-todo-honor-ignore-options t)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
      '((daily today require-timed)
	"----------------"
	(800 1200 1600 2000)))

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Some helper functions for selection within agenda views
(defun gs/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
	   (save-excursion (or (outline-next-heading)
			       (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))

(defun gs/select-projects ()
  "Selects tasks which are project headers"
  (gs/select-with-tag-function #'bh/is-project-p))

(defun gs/select-standalone-tasks ()
  "Skips tags which belong to projects. Is neither a project, nor does it blong to a project"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (not (bh/is-project-subtree-p))))))

(defun gs/select-projects-and-standalone-tasks ()
  "Skips tags which are not projects"
  (gs/select-with-tag-function
   #'(lambda () (or
		 (bh/is-project-p)
		 (bh/is-project-subtree-p)))))

(defun gs/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (bh/is-project-subtree-p)))))

;; == Agenda Post-processing ==
;; Highlight the "!!" for stuck projects (for emphasis)
(defun gs/org-agenda-project-highlight-warning ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "!W" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "orange"))
	))
    (goto-char (point-min))
    (while (re-search-forward "!S" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "white" :background "red"))
	))
    ))
(add-hook 'org-finalize-agenda-hook 'gs/org-agenda-project-highlight-warning)


;; == Agenda Navigation ==

;; Search for a "=" and go to the next line
(defun gs/org-agenda-next-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (if (search-forward "" nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

;; Search for a "=" and go to the previous line
(defun gs/org-agenda-prev-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (forward-line -2)
  (if (search-forward "" nil t -1)
      (forward-line 1)
    (goto-char (point-min))))

;; Bind the keys
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (define-key org-agenda-mode-map (kbd "M-n") 'gs/org-agenda-next-section)
	    (define-key org-agenda-mode-map (kbd "M-p") 'gs/org-agenda-prev-section)))
