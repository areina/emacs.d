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
      (erc :server "irc.freenode.net"
	   :port 6667
	   :nick private-irc-freenode-username
	   :password private-irc-freenode-password)))
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

(defun endless/mark-read ()
  "Mark buffer as read up to current line."
  (message "foo")
  (let ((inhibit-read-only t))
    (put-text-property
     (point-min) (- (line-beginning-position) 5)
     'face       'font-lock-comment-face)))

(defun endless/bury-buffer ()
  "Bury buffer and maybe close its window."
  (interactive)
  (message "foo")
  (endless/mark-read)
  (bury-buffer)
  (when (cdr (window-list nil 'nomini))
    (delete-window)))

(eval-after-load 'erc
  '(define-key erc-mode-map (kbd "<escape>")
     #'endless/bury-buffer))

(defun erc-track-switch-buffer (arg)
  "Switch to the next active ERC buffer, or if there are no active buffers,
switch back to the last non-ERC buffer visited.  Next is defined by
`erc-track-switch-direction', a negative argument will reverse this."
  (interactive "p")
  (if (not erc-track-mode)
      (message (concat "Enable the ERC track module if you want to use the"
		       " tracking minor mode"))
    (cond (erc-modified-channels-alist
	   ;; if we're not in erc-mode, set this buffer to return to
	   (unless (eq major-mode 'erc-mode)
	     (setq erc-track-last-non-erc-buffer (current-buffer)))
	   ;; hack
	   (when (eq major-mode 'erc-mode)
	     (endless/bury-buffer))
	   ;; and jump to the next active channel
	   (switch-to-buffer (erc-track-get-active-buffer arg)))
	  ;; if no active channels, switch back to what we were doing before
	  ((and erc-track-last-non-erc-buffer
		erc-track-switch-from-erc
		(buffer-live-p erc-track-last-non-erc-buffer))
	   ;; hack
	   (when (eq major-mode 'erc-mode)
	     (endless/bury-buffer))
	   (switch-to-buffer erc-track-last-non-erc-buffer)))))

(defun erc-track-reset ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

;;
;; JABBER
;;

(use-package jabber
  :ensure t
  :defer t
  :config
  (setq jabber-chat-buffer-show-avatar nil
	jabber-vcard-avatars-retrieve nil
	jabber-alert-presence-hooks nil
	jabber-account-list `(("areina0@gmail.com"
			       (:network-server . "talk.google.com")
			       (:password . ,private-gmail-jabber-passwd)
			       (:connection-type . ssl)))))

(provide '02chat)
;;; 02chat.el ends here
