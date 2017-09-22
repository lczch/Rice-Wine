;; `use-package' examples

;; `:commands'
(use-package org
  :init
  (rw-add-to-load-path (expand-file-name "org-mode/lisp" rice-wine-git-package-dir))
  (rw-add-to-load-path (expand-file-name "org-mode/contrib/lisp" rice-wine-git-package-dir))
  :mode (("\\.org\\'" . org-mode))
  :commands (org-mode)
  :config
  (use-package init-org)
  )

(use-package lisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.asd\\'" . lisp-mode))
  :config
  (defvar elisp-company-backends
    '(company-elisp
      ;; 在笔记本上用时很慢, 不知道为什么...
      ;; company-files
      ))

  (defvar slime-company-backends
    '(company-slime company-files))
  
  (defun rice-wine-emacs-lisp-func ()
    (rice-wine-lisp-func)
    (eldoc-mode)
    (setup-company-mode elisp-company-backends)
    (local-set-key (kbd "C-x C-e") 'pp-eval-last-sexp))

  (defun rice-wine-common-lisp-func ()
    (rice-wine-lisp-func)
    (eldoc-mode)
    (slime-mode)
    (local-set-key (kbd "C-j") 'slime-eval-print-last-expression)
    (setup-company-mode slime-company-backends)
    ;; (setq indent-line-function 'common-lisp-indent-function)
    ;; (put 'if 'common-lisp-indent-function 2)
    )

  (add-hook 'emacs-lisp-mode-hook 'rice-wine-emacs-lisp-func)
  (add-hook 'lisp-mode-hook 'rice-wine-common-lisp-func))
;; 
