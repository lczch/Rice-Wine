;;; coding environment of lisp

;; paredit
(require 'paredit)
(defun turn-on-paredit ()
  (paredit-mode +1))

;; eldoc
(require 'eldoc)
(setq eldoc-idle-delay 0.2)
(setq eldoc-echo-area-use-multiline-p t)
(defun turn-on-eldoc ()
  (turn-on-eldoc-mode))

;; rainbow-delimiters
(require 'rainbow-delimiters)
(defun turn-on-rainbow-delimiters ()
  (rainbow-delimiters-mode t))

;;  my lisp-hook
(add-hook 'rice-wine-lisp-hook 'turn-on-paredit)
(add-hook 'rice-wine-lisp-hook 'turn-on-eldoc)
(add-hook 'rice-wine-lisp-hook 'turn-on-rainbow-delimiters)

(defun run-rice-wine-lisp-hook ()
  (run-hooks 'rice-wine-lisp-hook))

;; specific lisp dialects
(require 'init-emacs-lisp)
(require 'init-common-lisp)



;; end of init-lisp
(provide 'init-lisp)