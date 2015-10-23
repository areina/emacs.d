;;; 02chat.el --- Chat settings.
;;
;;; Commentary:
;; - ERC settings. Manage slack & freenode accounts. Get some values from secrets.el
;; - Jabber settings. Manage gtalk account.
;;
;;; Code:

(require 'use-package)
(require 's)
(require 'secrets)
;;
;; ERC
;;

(use-package erc
  :defer t
  :init
  (progn
    (use-package erc-join
      :config
      (progn
      (erc-autojoin-mode t)
      (setq erc-join-buffer 'bury
	    erc-autojoin-channels-alist
	    (list `(".*\\.freenode.net" ,@private-irc-freenode-auto-join-channels)))))

    (use-package erc-track
      :config
      (progn
	(erc-track-mode t)
	(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
					"324" "329" "332" "333" "353" "477"))))

    (add-hook 'erc-mode-hook '(lambda ()
				(turn-off-smartparens-mode)))

    (defun start-irc ()
      "Connect to IRC."
      (interactive)
      (erc :server private-irc-bouncer-ip
	   :port 5000
	   :nick private-irc-bouncer-slack-username
	   :password private-irc-bouncer-slack-password)
      (erc :server private-irc-bouncer-ip
	   :port 5000
	   :nick private-irc-bouncer-freenode-username
	   :password private-irc-bouncer-freenode-password)))
  :config
  (progn
    (setq erc-prompt-for-nickserv-password nil
          erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
	  erc-auto-query 'buffer
	  erc-server-auto-reconnect t
	  erc-server-reconnect-attempts 5
	  erc-server-reconnect-timeout 3
	  erc-rename-buffers t)

    ;; highlighting nicks
    (use-package erc-hl-nicks
      :ensure t
      :init (add-to-list 'erc-modules 'hl-nicks))
    (use-package erc-services
      :init
      (progn
	(add-to-list 'erc-modules 'spelling)
	(erc-services-mode 1)
	(erc-spelling-mode 1)))
    (use-package erc-tweet
      :disabled t
      :init
      (progn
	(add-to-list 'erc-modules 'tweet)))
    (use-package erc-image
      :ensure t
      :defer t
      :init
      (progn
	(add-to-list 'erc-modules 'image)))
    (use-package erc-youtube
      :disabled t
      :init
      (progn
	(add-to-list 'erc-modules 'youtube)))
    (use-package erc-replace
      :config
      (progn
	(defvar toni-erc-clean-gh-new-branch-regex
	  "New branch \"https://.*/\\(.*\\)\" was pushed by \\(.*\\)")

	(defun toni-erc-clean-gh-new-branch (match)
	  ""
	  (let* ((matches (cdr (s-match toni-erc-clean-gh-new-branch-regex match)))
		 (branch (car matches))
		 (by (cadr matches)))
	    (format "new branch \"%s\" by %s" branch by)))

	(add-to-list 'erc-modules 'replace)
	(add-to-list 'erc-replace-alist '(toni-erc-clean-gh-new-branch-regex . toni-erc-clean-gh-new-branch))
	(add-hook 'erc-insert-modify-hook 'erc-replace-insert)
	(add-hook 'erc-send-modify-hook 'erc-replace-insert)))
    (erc-update-modules)))

;;
;; JABBER
;;

(use-package jabber
  :ensure t
  :defer t
  :config
  (progn
    (setq jabber-chat-buffer-show-avatar nil
	  jabber-vcard-avatars-retrieve nil
	  jabber-alert-presence-hooks nil
	  jabber-account-list `(("areina0@gmail.com"
				 (:network-server . "talk.google.com")
				 (:password . ,private-gmail-jabber-passwd)
				 (:connection-type . ssl))))))

(provide '02chat)
;;; 02chat.el ends here
