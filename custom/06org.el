;;; 06org.el --- Org-mode settings.
;;
;;; Commentary:
;;
;; Nothing interesting here...
;;
;;; Code:

(use-package org
  :defer t
  :config
  (progn
    (setq org-directory "~/.emacs.d/org"
	  org-default-notes-file "~/.emacs.d/org/refile.org"
	  org-agenda-files (quote ("~/.emacs.d/org/todo.org"
				   "~/.emacs.d/org/refmob.org"
				   "~/.emacs.d/org/refile.org"))
	  org-mobile-inbox-for-pull "~/.emacs.d/org/mobile.org"
	  org-mobile-directory "~/Dropbox/MobileOrg"
	  )))
(use-package org-capture
  :defer t)

(use-package org-protocol
  :config
  (progn
    (add-to-list 'org-protocol-protocol-alist
		 '("Subscribe to feed with elfeed."
		   :protocol "elfeed"
		   :function org-protocol-elfeed))))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/.emacs.d/org/refile.org")
               "* TODO %?\nSCHEDULED: %t\n")
              ("n" "note" entry (file "~/.emacs.d/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("w" "org-protocol" entry (file "~/.emacs.d/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t))))

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; (load-file (expand-file-name "vendor/org-protocol-github-lines.el" user-emacs-directory))
(setq org-protocol-github-project-directories '("/tmp/"))

;;;###autoload
(defun org-protocol-elfeed (data)
  (let ((url (org-protocol-sanitize-uri data)))
    (elfeed-add-feed url)
    (customize-save-variable 'elfeed-feeds elfeed-feeds)))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; NOT USING ORG-SYNC RIGHT NOW
;; (add-to-list 'load-path (expand-file-name "org-sync" user-emacs-directory))
;; (mapc 'load
;;       '("os" "os-bb" "os-github" "os-rmine"))
;; (setq os-github-auth '("areina" . "f08259fb6cbcb719cc62d393c3016ffde59a9507"))

(provide '06org)
;;; 06org.el ends here
