;; I always want return to perform newline automaticly
(define-key global-map (kbd "RET") 'newline-and-indent)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :config
  ;; (define-globalized-minor-mode rainbow-delimiters-global-mode
  ;;   rainbow-delimiters-mode
  ;;   rainbow-delimiters-mode)
  ;; (rainbow-delimiters-global-mode)
  ;; active rainbow-delimiters minor mode globally 
  )

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

;; smartparens
(use-package smartparens
  ;; :init
  ;; (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  :commands (smartparens-mode smartparens-strict-mode turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  (setq sp-autoskip-closing-pair 'always)
  (sp-use-smartparens-bindings)

  (sp-with-modes 'tuareg-mode
    ;; disable auto insert of "'" 
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  (sp-with-modes 'minibuffer-inactive-mode
    (sp-local-pair "'" nil :actions nil))
  )


;;------------------------------------------------------------------------------
;; common features shared by all programming language
;;------------------------------------------------------------------------------
(defun rice-wine-prog-func ()
  "common features of all programming mode"
  (rainbow-delimiters-mode)
  (fic-mode)
  (smartparens-mode)
  ;; (cscope-minor-mode)
  )

;;------------------------------------------------------------------------------
;; lisp: all languages belong to lisp or scheme
;;------------------------------------------------------------------------------
(use-package init-lisp)

;;------------------------------------------------------------------------------
;; coq
;;------------------------------------------------------------------------------
(use-package init-coq)

;;------------------------------------------------------------------------------
;; ruby
;;------------------------------------------------------------------------------
(use-package init-ruby)

;;------------------------------------------------------------------------------
;; c
;;------------------------------------------------------------------------------
(use-package init-cc-mode)

;;------------------------------------------------------------------------------
;; sh
;;------------------------------------------------------------------------------
(use-package init-sh-mode)

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
(use-package init-ocaml)

(provide 'init-programming)
