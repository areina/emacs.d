;;; 03email.el --- News and Email settings.
;;
;;; Commentary:
;;
;; - Manage work & personal accounts with mu4e.
;; - RSS feeds with Elfeed
;; - News/Mailing list with gnus
;;
;; sending mail -- make sure the gnutls command line utils are installed.
;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.
;;
;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'use-package)

(use-package netrc
  :defer t
  :config
  (setq netrc-file (expand-file-name "~/.authinfo.gpg"))
  :init
  (progn
    (defun offlineimap-get-password (host port)
      (cadr (netrc-credentials host port)))))

(use-package smtpmail
  :defer t
  :config
  (setq smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-stream-type 'starttls
	smtpmail-smtp-service 587))

(use-package gnus
  :ensure t
  :config
  (progn
    (setq gnus-select-method '(nntp "news.gmane.org"))
    (setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n")
    (setq gnus-sum-thread-tree-indent "  ")
    (setq gnus-sum-thread-tree-root "") ;; "\u25cf ")
    (setq gnus-sum-thread-tree-false-root "") ;; "\u25ef ")
    (setq gnus-sum-thread-tree-single-indent "") ;; "\u25ce ")
    (setq gnus-sum-thread-tree-vertical        "\u2502")
    (setq gnus-sum-thread-tree-leaf-with-other "\u251c\u2500\u25ba ")
    (setq gnus-sum-thread-tree-single-leaf     "\u2570\u2500\u25ba ")
    (setq gnus-summary-line-format
	  (concat
	   "%0{%U%R%z%}"
	   "%3{\u2502%}" "%1{%d%}" "%3{\u2502%}" ;; date
	   "  "
	   "%4{%-20,20f%}"               ;; name
	   "  "
	   "%3{\u2502%}"
	   " "
	   "%1{%B%}"
	   "%s\n"))
    (setq gnus-summary-display-arrow t)
    (setq gnus-fetch-old-headers 'nil)
    (setq gnus-asynchronous t)))

(use-package elfeed
  :ensure t
  :config
  (setf url-queue-timeout 120)
  (setq elfeed-search-title-max-width 160)
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  :init
  (progn
    (use-package elfeed-org
      :ensure t
      :config
      (progn
	(elfeed-org)))))

(use-package mu4e
  :defer t
  :commands mu4e
  :config
  (progn
    ;; don't save message to Sent Messages, GMail/IMAP will take care of this
    (setq mu4e-sent-messages-behavior 'delete)
    (setq mu4e-trash-folder 'custom-mu4e-trash-folder)

    ;; attempt to show images when viewing messages
    (setq mu4e-view-show-images t
	  mu4e-show-images t
	  mu4e-view-image-max-width 800
	  mu4e-compose-complete-only-personal t
	  mu4e-compose-complete-only-after "2014-01-01")

    ;; (setq mu4e-html2text-command "html2text -utf8 -width 72") ;; nil "Shel command that converts HTML
    ;; ref: http://emacs.stackexchange.com/questions/3051/how-can-i-use-eww-as-a-renderer-for-mu4e
    (defun my-render-html-message ()
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
	(erase-buffer)
	(shr-insert-document dom)
	(goto-char (point-min))))

    (setq mu4e-html2text-command 'my-render-html-message)

    ;; give me ISO(ish) format date-time stamps in the header list
    (setq  mu4e-headers-date-format "%d-%m-%Y %H:%M")

    ;; the headers to show in the headers list -- a pair of a field
    ;; and its width, with `nil' meaning 'unlimited'
    ;; (better only use that for the last field.
    ;; These are the defaults:
    (setq mu4e-headers-fields
	  '( (:date          .  20)
	     (:flags         .   6)
	     (:from          .  22)
	     (:subject       .  nil)))

    (setq my-mu4e-account-alist
	  '(("areina0@gmail.com"
	     (mu4e-sent-folder "/areina0@gmail.com/[Gmail].Sent Mail")
	     (mu4e-drafts-folder "/areina0@gmail.com/[Gmail].Drafts")
	     (mu4e-trash-folder  "/areina0@gmail.com/[Gmail].Trash")
	     (user-mail-address "areina0@gmail.com")
	     (smtpmail-smtp-user "areina0@gmail.com"))
	    ("toni@3scale.net"
	     (mu4e-sent-folder "/toni@3scale.net/[Gmail].Sent Mail")
	     (mu4e-drafts-folder "/toni@3scale.net/[Gmail].Drafts")
	     (mu4e-trash-folder  "/toni@3scale.net/[Gmail].Trash")
	     (user-mail-address "toni@3scale.net")
	     (smtpmail-smtp-user "toni@3scale.net"))))
    (setq mu4e-maildir (expand-file-name "~/Maildir")
	  mu4e-compose-signature nil
	  mu4e-user-mail-address-list '("areina0@gmail.com" "toni@3scale.net")
	  mu4e-get-mail-command "true"
	  mu4e-compose-dont-reply-to-self t
	  mu4e-update-interval  120
	  mu4e-sent-folder "/areina0@gmail.com/[Gmail].Sent Mail"
	  mu4e-drafts-folder "/areina0@gmail.com/[Gmail].Drafts"
	  mu4e-trash-folder  "/areina0@gmail.com/[Gmail].Trash"
	  message-send-mail-function 'smtpmail-send-it
	  user-full-name "Toni Reina"))
  :init
  (progn
    (use-package mu4e-maildirs-extension
      :ensure t
      :disabled t
      :config (mu4e-maildirs-extension))

    (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

    (defconst mu4e~view-url-regexp
      "\\(\\(https?\\://\\|mailto:\\)[-+\[:alnum:\].?_$%/+&#@!*~,:;=/()]+\[[:alnum:]\]\\)"
      "Regexp that matches http:/https:/mailto: URLs; match-string 1
will contain the matched URL, if any.")

    (defun clean-my-mu4e-inbox ()
      (interactive)
      (mapcar (lambda (x) (apply 'move-emails-by-pattern x)) private-3scale-inbox-rules))

    (defun move-emails-by-pattern (target pattern field)
      (let ((markpair (cons 'move target)))
	(mu4e-headers-mark-for-each-if markpair (lambda (msg param)
						  (let* ((do-mark) (value (mu4e-msg-field msg field)))
						    (setq do-mark
							  (if (member field '(:to :from :cc :bcc :reply-to))
							      (find-if (lambda (contact)
									 (let ((name (car contact)) (email (cdr contact)))
									   (or (and name (string-match pattern name))
									       (and email (string-match pattern email))))) value)
							    (string-match pattern (or value "")))))))))

    (defun my-mu4e-set-account ()
      "Set the account for composing a message."
      (let* ((account
	      (if mu4e-compose-parent-message
		  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		    (string-match "/\\(.*?\\)/" maildir)
		    (match-string 1 maildir))
		(completing-read (format "Compose with account: (%s) "
					 (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
				 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
				 nil t nil nil (caar my-mu4e-account-alist))))
	     (account-vars (cdr (assoc account my-mu4e-account-alist))))
	(if account-vars
	    (mapc #'(lambda (var)
		      (set (car var) (cadr var)))
		  account-vars)
	  (error "No email account found"))))

    (defun custom-mu4e-trash-folder (msg)
      "Return the valid trash folder for MSG."
      (if msg
	  (cond
	   ((string-match "areina0@gmail.com" (mu4e-message-field msg :maildir))
	    "/areina0@gmail.com/[Gmail].Trash")
	   ((string-match "toni@3scale.net" (mu4e-message-field msg :maildir))
	    "/toni@3scale.net/[Gmail].Trash"))
	(mu4e-ask-maildir-check-exists "Save message to maildir: ")))))

(provide '03email)
;;; 03email.el ends here
