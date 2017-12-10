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
  :defer t
  :init
  (progn
    (setq url-queue-timeout 120
          elfeed-search-title-max-width 160
          elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
          elfeed-use-curl t)
    (defun toni-elfeed-random-entry ()
      (interactive)
      (with-current-buffer (elfeed-search-buffer)
        (goto-char (point-min))
        (let ((num-lines (- (count-lines (point-min) (point-max)) 2)))
          (forward-line (random num-lines))
          (elfeed-search-show-entry (elfeed-search-selected :single)))))))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (progn
    (elfeed-org)))

(use-package mu4e
  :defer t
  :commands mu4e
  :config
  (progn
    ;; (add-hook 'message-setup-hook 'mml-secure-message-sign-encrypt)
    ;; don't save message to Sent Messages, GMail/IMAP will take care of this
    (setq mu4e-trash-folder 'custom-mu4e-trash-folder
          mu4e-compose-complete-only-personal t
          mu4e-compose-complete-only-after "2014-01-01"
          mu4e-compose-in-new-frame t
          mu4e-compose-context-policy 'ask-if-none
          mu4e-view-show-addresses t
          mml-secure-openpgp-signers '("7BC12868")
          mu4e-html2text-command 'toni-mu4e-shr2text
          mu4e-headers-date-format "%d-%m-%Y %H:%M"

          mu4e-headers-fields '((:date          .  20)
                                (:flags         .   6)
                                (:from          .  22)
                                (:subject       .  nil))

          message-send-mail-function 'smtpmail-send-it
          mu4e-maildir "~/Maildir")

    (setq mu4e-org-contacts-file  (expand-file-name "contacts.org" org-directory))
    (add-to-list 'mu4e-headers-actions
                 '("org-contact-add" . mu4e-action-add-org-contact) t)
    (add-to-list 'mu4e-view-actions
                 '("org-contact-add" . mu4e-action-add-org-contact) t)

    (setq mu4e-contexts
          `( ,(make-mu4e-context
               :name "Gmail"
               :enter-func (lambda ()
                             (mu4e-message "Switch to the Gmail account context"))
               ;; leave-func not defined
               :match-func (lambda (msg)
                             (when msg
                               (mu4e-message-contact-field-matches msg
                                                                   :to "areina0@gmail.com")))
               :vars '((user-mail-address . "areina0@gmail.com")
                       (user-full-name . "Toni Reina")
                       (mu4e-sent-messages-behavior . delete)
                       (smtpmail-smtp-user . "areina0@gmail.com")
                       (mu4e-drafts-folder . "/areina0@gmail.com/[Gmail].Drafts")
                       (mu4e-sent-folder . "/areina0@gmail.com/[Gmail].Sent Mail")
                       (mu4e-trash-folder . "/areina0@gmail.com/[Gmail].Trash")
                       (mu4e-compose-signature . nil)
                       (mu4e-maildir-shortcuts . (("/areina0@gmail.com/INBOX" . ?i)))))
             ,(make-mu4e-context
               :name "Riseup"
               :enter-func (lambda ()
                             (mu4e-message "Switch to the Riseup account context"))
               ;; leave-fun not defined
               :match-func (lambda (msg)
                             (when msg
                               (mu4e-message-contact-field-matches msg
                                                                   :to "areina@riseup.net")))
               :vars `((user-mail-address . "areina@riseup.net" )
                       (user-full-name . "Toni Reina")
                       (mu4e-sent-messages-behavior . sent)
                       (mu4e-drafts-folder . "/areina@riseup.net/Drafts")
                       (mu4e-sent-folder . "/areina@riseup.net/Sent")
                       (mu4e-trash-folder . "/areina@riseup.net/Trash")
                       (smtpmail-smtp-user . "areina@riseup.net")
                       (smtpmail-smtp-server . "mail.riseup.net")
                       (mu4e-maildir-shortcuts . (("/areina@riseup.net/INBOX" . ?i)))
                       (mu4e-compose-signature . nil))))))
  :init
  (progn
    (use-package mu4e-maildirs-extension
      :ensure t
      :disabled t
      :config (mu4e-maildirs-extension))

    (defconst mu4e~view-url-regexp
      "\\(\\(https?\\://\\|mailto:\\)[-+\[:alnum:\].?_$%/+&#@!*~,:;=/()]+\[[:alnum:]\]\\)"
      "Regexp that matches http:/https:/mailto: URLs; match-string 1
will contain the matched URL, if any.")

    (defun toni-mu4e-shr2text ()
      "Html to text using the shr engine; this can be used in
`mu4e-html2text-command' in a new enough emacs. Based on code by
Titus von der Malsburg."
      (interactive)
      (let ((dom (libxml-parse-html-region (point-min) (point-max)))
            ;; When HTML emails contain references to remote images,
            ;; retrieving these images leaks information. For example,
            ;; the sender can see when I openend the email and from which
            ;; computer (IP address). For this reason, it is preferrable
            ;; to not retrieve images.
            ;; See this discussion on mu-discuss:
            ;; https://groups.google.com/forum/#!topic/mu-discuss/gr1cwNNZnXo
            (shr-inhibit-images t)
            ;; Avoid colors in html emails.
            (shr-use-colors nil))
        (erase-buffer)
        (shr-insert-document dom)
        (goto-char (point-min))))

    (defun clean-my-mu4e-inbox ()
      (interactive)
      (mapcar (lambda (x) (apply 'move-emails-by-pattern x)) private-clean-inbox-rules))

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
