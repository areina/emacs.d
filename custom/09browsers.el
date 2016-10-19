;;; 09browsers.el --- Browsers settings
;;
;;; Commentary:
;;
;;; Code:

;; Set default browser

(use-package browse-url
  :config
  (setq browse-url-generic-program (executable-find "conkeror")
	browse-url-browser-function '(("github.com" . browse-url-conkeror)
				      ("docs.google.com" . browse-url-chromium)
				      ("." . eww-browse-url))))

(use-package eww
  :ensure t
  :config
  (setq shr-use-fonts nil
	shr-inhibit-images t
	shr-external-browser 'browse-url-generic)
  :init
  (progn
    (use-package :shr-color
      :config
      (setq shr-color-visible-luminance-min 85))
    (defun oww-down (arg)
      (interactive "p")
      (if (bolp)
	  (progn
	    (forward-paragraph arg)
	    (forward-line 1))
	(line-move arg)))

    (defun oww-up (arg)
      (interactive "p")
      (if (bolp)
	  (progn
	    (forward-line -1)
	    (backward-paragraph arg)
	    (forward-line 1))
	(line-move (- arg))))

    (defun oww-mode-hook ()
      (define-key eww-mode-map "o" 'eww)
      (define-key eww-mode-map "n" 'oww-down)
      (define-key eww-mode-map "N" 'eww-next-url)
      (define-key eww-mode-map "p" 'oww-up)
      (define-key eww-mode-map "P" 'eww-previous-url)
      (define-key eww-mode-map "v" 'recenter-top-bottom)
      (define-key eww-mode-map "V" 'eww-view-source))

    (add-hook 'eww-mode-hook 'oww-mode-hook)
    (use-package eww-lnum
      :ensure t
      :defer t
      :init
      (progn
	(with-eval-after-load 'eww
	  (define-key eww-mode-map "f" (lambda() (interactive) (eww-lnum-follow 4)))
	  (define-key eww-mode-map "F" 'eww-lnum-universal))))))
