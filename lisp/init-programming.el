(require 'paredit)

(defun turn-on-paredit ()
  (paredit-mode +1))

(require 'eldoc)

(defun turn-on-eldoc ()
  (turn-on-eldoc-mode))

(require 'rainbow-delimiters)

(defun turn-on-rainbow-delimiters ()
  (rainbow-delimiters-mode t))

(add-hook 'rice-wine-lisp-hook 'turn-on-paredit)
(add-hook 'rice-wine-lisp-hook 'turn-on-eldoc)
(add-hook 'rice-wine-lisp-hook 'turn-on-rainbow-delimiters)

(defun run-rice-wine-lisp-hook ()
  (run-hooks 'rice-wine-lisp-hook))

(require 'init-common-lisp)

(provide 'init-programming)
