;;; 03email.el --- Email settings.
;;
;;; Commentary:
;;
;; - Manage work & personal accounts with mu4e.
;;
;; sending mail -- make sure the gnutls command line utils are installed.
;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.
;;
;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'smtpmail)
(require 'netrc)

(mu4e-maildirs-extension)

(add-hook 'mu4e-main-mode-hook
	  (lambda()
	    (setq show-trailing-whitespace nil)))

(add-hook 'mu4e-headers-mode-hook
	  (lambda()
	    (setq show-trailing-whitespace nil)))

;; default
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
      user-full-name "Toni Reina"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

(defvar my-mu4e-account-alist
  '(("areina0@gmail.com"
     (mu4e-sent-folder "/areina0@gmail.com/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/areina0@gmail.com/[Gmail].Drafts")
     (mu4e-trash-folder  "/areina0@gmail.com/[Gmail].Trash")
     (user-mail-address "areina0@gmail.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-user "areina0@gmail.com"))
    ("toni@3scale.net"
     (mu4e-sent-folder "/toni@3scale.net/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/toni@3scale.net/[Gmail].Drafts")
     (mu4e-trash-folder  "/toni@3scale.net/[Gmail].Trash")
     (user-mail-address "toni@3scale.net")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-user "toni@3scale.net"))))

(defun custom-mu4e-trash-folder (msg)
  "Return the valid trash folder for MSG."
  (if msg
      (cond
       ((string-match "areina0@gmail.com" (mu4e-message-field msg :maildir))
        "/areina0@gmail.com/[Gmail].Trash")
       ((string-match "toni@3scale.net" (mu4e-message-field msg :maildir))
        "/toni@3scale.net/[Gmail].Trash"))
    (mu4e-ask-maildir-check-exists "Save message to maildir: ")))

(setq mu4e-trash-folder 'custom-mu4e-trash-folder)

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

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (setq mu4e-html2text-command "html2text | grep -v '&nbsp_place_holder;'")

(defconst mu4e~view-url-regexp
  "\\(\\(https?\\://\\|mailto:\\)[-+\[:alnum:\].?_$%/+&#@!*~,:;=/()]+\[[:alnum:]\]\\)"
  "Regexp that matches http:/https:/mailto: URLs; match-string 1
will contain the matched URL, if any.")

(defun offlineimap-get-password (host port)
  (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
	 (hostentry (netrc-machine netrc host port port)))
    (when hostentry (netrc-get hostentry "password"))))

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


(provide '03email)
;;; 03email.el ends here
