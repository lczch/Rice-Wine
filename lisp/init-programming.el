;; I always want return to perform newline automaticly
(define-key global-map (kbd "RET") 'newline-and-indent)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(defun turn-on-rainbow-delimiters ()
  (rainbow-delimiters-mode t))
(add-hook 'rice-wine-prog-hook 'turn-on-rainbow-delimiters)

;; fic-mode: highlight TODO/FIXME/BUG in comment
(require 'fic-mode)
(defun turn-on-fic-mode ()
  (fic-mode 1))
(add-hook 'rice-wine-prog-hook 'turn-on-fic-mode)

;; subword-mode: "camelCase" is two word
;; superwode-mode: "camelCase" is a whole word
(defun turn-on-subword-mode ()
  (subword-mode 1))
(add-hook 'rice-wine-prog-hook 'turn-on-subword-mode)

;; electric-pair-mode
(defun turn-on-electric-pair-mode ()
  (electric-pair-mode 1))
(add-hook 'rice-wine-prog-hook 'turn-on-electric-pair-mode)

;; eldoc
(require 'eldoc)
(setq eldoc-idle-delay 0.2)
(setq eldoc-echo-area-use-multiline-p t)
(defun turn-on-eldoc ()
  (turn-on-eldoc-mode))
(add-hook 'rice-wine-prog-hook 'turn-on-eldoc)

;; show trailing whitspace
(defun turn-on-show-trailing-whitspace ()
  (setq show-trailing-whitespace t))
(add-hook 'rice-wine-prog-hook 'turn-on-show-trailing-whitspace)

(defun run-rice-wine-prog-hook ()
  (run-hooks 'rice-wine-prog-hook))

(require 'init-lisp)

(require 'init-coq)

(provide 'init-programming)
