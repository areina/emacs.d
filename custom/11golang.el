;;; 11golang.el --- Golang stuff.
;;
;;; Commentary:
;;
;;; Code:

(use-package go-mode
  :ensure t
  :bind (("M-." . godef-jump)
         ("M-," . pop-tag-mark))
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save nil 'local)
                (subword-mode 1)
                (if (not (and (stringp compile-command)
                              ;; so that we can have a per-project setting too
                              (string-match "go" compile-command)))
                    (set (make-local-variable 'compile-command)
                         "go build -v && go test -v && go vet && golint"))))))

(use-package go-eldoc
  :ensure t)

(use-package go-projectile
  :ensure t
  :config
  (progn
    (with-eval-after-load 'go-mode
      (go-projectile-set-gopath)
      (go-projectile-tools-add-path))))

(use-package company-go
  :ensure t
  :config
  (progn
    (setq company-go-show-annotation t)
    (add-to-list 'company-backends 'company-go)))

(use-package gorepl-mode
  :ensure t
  :diminish gorepl-mode
  :init (add-hook 'go-mode-hook #'gorepl-mode))

(use-package go-dlv
  :ensure t)

;; go get -u github.com/alecthomas/gometalinter
;; gometalinter --install --update
(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (flycheck-gometalinter-setup)))

(provide '11golang)
;;; 11golang.el ends here
