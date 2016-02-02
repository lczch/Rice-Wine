;; configure company for elisp

(defun elisp-setup-company-mode ()
  "company specific setup for elisp"
  (setup-company-mode '((company-elisp company-dabbrev))))

(add-hook 'emacs-lisp-mode-hook 'elisp-setup-company-mode)
(add-hook 'emacs-lisp-mode-hook 'run-rice-wine-lisp-hook)


(provide 'init-emacs-lisp)