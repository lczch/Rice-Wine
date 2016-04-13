;; I always want return to perform newline automaticly
(define-key global-map (kbd "RET") 'newline-and-indent)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (defun rainbow-on ()
    (interactive)
    (rainbow-delimiters-mode 1))
  (defun rainbow-off ()
    (interactive)
    (rainbow-delimiters-mode 0)))

;; fic-mode: highlight TODO/FIXME/BUG in comment
(use-package fic-mode
  :commands fic-mode
  :init
  (defun fic-on ()
    (interactive)
    (fic-mode 1))
  (defun fic-off ()
    (interactive)
    (fic-mode 0)))

;; subword-mode: "camelCase" is two word
;; superwode-mode: "camelCase" is a whole word
(use-package subword-mode
  :commands subword-mode
  :init
  (defun subword-on ()
    (interactive)
    (subword-mode 1))
  (defun subword-off ()
    (interactive)
    (subword-mode 0)))

;; show trailing whitspace
(defun trailing-whitspace-on ()
  (setq show-trailing-whitespace t))
(defun trailing-whitspace-off ()
  (setq show-trailing-whitespace nil))

(defun rice-wine-prog-func ()
  "common features of all programming mode"
  (rainbow-on)
  (fic-on)
  (subword-on))

(require 'init-lisp)

(require 'init-coq)

;; (require 'init-c)

(provide 'init-programming)
