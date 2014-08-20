;;; 02chat.el --- Chat settings.
;;
;;; Commentary:
;; - ERC settings. Manage slack & freenode accounts. Get some values from secrets.el
;; - Jabber settings. Manage gtalk account.
;;
;;; Code:

(require 'use-package)

;;
;; ERC
;;

(require 'erc-spelling)
(erc-spelling-mode 1)

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "3scale.irc.slack.com" :port 6667
           :nick "toni" :password slack-nick-passwd :full-name "toni")
  (erc :server "irc.freenode.net" :port 6667
       :nick "kablaam" :password irc-freenode-nick-passwd :full-name "kablaam"))

(erc-autojoin-mode t)
(setq erc-auto-query 'buffer)

(setq erc-autojoin-channels-alist (list `(".*\\.freenode.net" ,@irc-freenode-auto-join-channels)))

(setq erc-server-auto-reconnect t)
(setq erc-server-reconnect-attempts 5)
(setq erc-server-reconnect-timeout 3)

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"))

(add-hook 'erc-mode-hook '(lambda ()
			    (turn-off-smartparens-mode)))

;;
;; JABBER
;;

(use-package jabber
  :config
  (progn
    (setq jabber-account-list
	  '(("areina0@gmail.com"
	     (:network-server . "talk.google.com")
	     (:connection-type . ssl))))
    (setq jabber-vcard-avatars-retrieve nil)
    (setq jabber-chat-buffer-show-avatar nil)))

(provide '02chat)
;;; 02chat.el ends here
