;; I always want return to perform newline automaticly
(define-key global-map (kbd "RET") 'newline-and-indent)

;; fic-mode: highlight TODO/FIXME/BUG in comment
(use-package fic-mode
  :commands fic-mode)

;; subword-mode: "camelCase" is two word
;; superwode-mode: "camelCase" is a whole word
(use-package subword-mode
  :commands subword-mode)

;; show trailing whitspace
(defun trailing-whitspace-on ()
  (setq show-trailing-whitespace t))
(defun trailing-whitspace-off ()
  (setq show-trailing-whitespace nil))

;; eldoc mode make coq-mode extensive low !!!!!!!
(use-package eldoc
  :commands (eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t))

;;------------------------------------------------------------------------------
;; common features shared by all programming language
;;------------------------------------------------------------------------------
(defun rice-wine-prog-func ()
  "common features of all programming mode"
  ;; (rainbow-delimiters-mode)
  (fic-mode)
  ;; (smartparens-mode)
  ;; (cscope-minor-mode)
  )

;;------------------------------------------------------------------------------
;; lisp: all languages belong to lisp or scheme
;;------------------------------------------------------------------------------
(require 'init-lisp)

;;------------------------------------------------------------------------------
;; coq
;;------------------------------------------------------------------------------
(require 'init-coq)
;;------------------------------------------------------------------------------
;; ruby
;;------------------------------------------------------------------------------
(require 'init-ruby)

;;------------------------------------------------------------------------------
;; c
;;------------------------------------------------------------------------------
(require 'init-cc-mode)

;;------------------------------------------------------------------------------
;; sh
;;------------------------------------------------------------------------------
(require 'init-sh-mode)

;;------------------------------------------------------------------------------
;; asm (for sparc)
;;------------------------------------------------------------------------------
(use-package asm-mode
  :mode (("\\.S'" . asm-mode))
  :commands (asm-mode)
  :config
  ;; for sparc asm, which I always use
  (setq asm-comment-char ?\!)

  (defun asm-mode-func ()
    )

  (add-hook 'asm-mode-hook 'asm-mode-func)
  )

;;------------------------------------------------------------------------------
;; ocaml
;;------------------------------------------------------------------------------
;; (use-package init-ocaml)

(require 'init-rust)

(require 'init-lua)

(require 'init-python)

(provide 'init-programming)
