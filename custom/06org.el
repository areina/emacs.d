;;; 06org.el --- Org-mode settings.
;;
;;; Commentary:
;;
;; Nothing interesting here...
;;
;;; Code:

(use-package org
  :defer t
  :bind (("C-c c" . org-capture))
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
  :config
  (setq org-capture-templates
	(quote (("t" "todo" entry (file "~/.emacs.d/org/refile.org")
		 "* TODO %?\nSCHEDULED: %t\n")
		("n" "note" entry (file "~/.emacs.d/org/refile.org")
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("w" "org-protocol" entry (file "~/.emacs.d/org/refile.org")
		 "* TODO Review %c\n%U\n" :immediate-finish t)))))

(use-package org-protocol
  :config
  (progn
    (add-to-list 'org-protocol-protocol-alist
		 '("Subscribe to feed with elfeed."
		   :protocol "elfeed"
		   :function org-protocol-elfeed))
    (add-to-list 'org-protocol-protocol-alist
		 '("Open url with eww."
		   :protocol "eww"
		   :function org-protocol-eww))))

;; (load-file (expand-file-name "vendor/org-protocol-github-lines.el" user-emacs-directory))
;; (validate-setq org-protocol-github-project-directories '("/tmp/"))

;;;###autoload
(defun org-protocol-elfeed (data)
  (let ((url (org-protocol-sanitize-uri data)))
    (elfeed-add-feed url)
    (customize-save-variable 'elfeed-feeds elfeed-feeds)))

;;;###autoload
(defun org-protocol-eww (data)
  (let ((url (org-protocol-sanitize-uri data)))
    (eww-browse-url url)
    (raise-frame)))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(provide '06org)
;;; 06org.el ends here
