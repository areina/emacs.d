;;; 04ruby.el --- Ruby stuff.
;;
;;; Commentary:
;;
;;; Code:

(use-package highlight-indentation
  :ensure t
  :init
  (progn
    (highlight-indentation-current-column-mode))
  :config
  (progn
    (set-face-background 'highlight-indentation-current-column-face "#073642")))

(use-package ruby-tools
  :diminish ruby-tools-mode
  :ensure t
  :after ruby-mode)

(use-package ruby-hash-syntax
  :ensure t
  :after ruby-mode)

(use-package feature-mode
  :ensure t
  :disabled t
  :after ruby-mode)

(use-package slim-mode
  :ensure t
  :disabled t
  :after ruby-mode)

(use-package yard-mode
  :ensure t
  :diminish yard-mode
  :disabled t
  :after ruby-mode
  :hook ruby-mode)

(use-package robe
  :defer t
  :ensure t
  :disabled t
  :after ruby-mode
  :init
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-robe))))

(use-package rspec-mode
  :ensure t
  :after ruby-mode
  :disabled t
  :init
  (progn
    (setq rspec-use-rake-flag nil))
  :config
  (progn
    (defadvice rspec-compile (around rspec-compile-around activate)
      "Use BASH shell for running the specs because of ZSH issues."
      (let ((shell-file-name "/bin/bash"))
        ad-do-it))))

(use-package rhtml-mode
  :ensure t
  :after ruby-mode
  :mode (("\\.rhtml$" . rhtml-mode)
         ("\\.html\\.erb$" . rhtml-mode)))

(use-package ruby-mode
  :ensure t
  :init
  (progn
    (setq ruby-deep-indent-paren nil))
  :config
  (progn
    (defconst align-ruby-modes '(ruby-mode)
      "align-ruby-modes is a variable defined in `align.el'.")

    (defconst ruby-align-rules-list
      '((ruby-comma-delimiter
         (regexp . ",\\(\\s-*\\)[^/ \t\n]")
         (modes . align-ruby-modes)
         (repeat . t))
        (ruby-string-after-func
         (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
         (modes . align-ruby-modes)
         (repeat . t))
        (ruby-symbol-after-func
         (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
         (modes . align-ruby-modes)))
      "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")

    (with-eval-after-load 'align
      (dolist (it ruby-align-rules-list)
        (add-to-list 'align-rules-list it))))

  :bind (("C-M-h" . backward-kill-word)
         ("C-M-n" . scroll-up-five)
         ("C-M-p" . scroll-down-five))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("\\.cap$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(provide '04ruby)
;;; 04ruby.el ends here
