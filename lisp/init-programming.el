;; I always want return to perform newline automaticly
(define-key global-map (kbd "RET") 'newline-and-indent)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(defun turn-on-rainbow-delimiters ()
  (rainbow-delimiters-mode t))

(add-hook 'rice-wine-prog-hook 'turn-on-rainbow-delimiters)

(defun run-rice-wine-prog-hook ()
  (run-hooks 'rice-wine-prog-hook))

(require 'init-lisp)

(require 'init-coq)

(provide 'init-programming)
