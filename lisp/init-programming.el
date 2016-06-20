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

;; smartparens
(use-package smartparens
  :init
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  :commands (smartparens-on smartparens-off turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (defun smartparens-on ()
    (interactive)
    (smartparens-mode t))

  (defun smartparens-off ()
    (interactive)
    (smartparens-mode nil))

  (sp-use-smartparens-bindings)
  ;; (sp-with-modes '(coq-mode)
  ;;   (sp-local-pair "Lemma" "Qed."))
  )


(defun rice-wine-prog-func ()
  "common features of all programming mode"
  (rainbow-on)
  (fic-on)
  ;;  (subword-on)
  )


;;------------------------------------------------------------------------------
;; lisp families
;;------------------------------------------------------------------------------
(use-package paredit
  :commands (paredit-on paredit-off)
  :config
  (defun paredit-on ()
    (interactive)
    (paredit-mode 1))
  (defun paredit-off ()
    (interactive)
    (paredit-mode 0))
  
  (define-key paredit-mode-map [remap kill-line] 'paredit-kill)
  (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp)
  (define-key paredit-mode-map (kbd "RET") 'paredit-newline)
  (define-key paredit-mode-map [remap backward-kill-sentence] nil))

;; eldoc mode make coq-mode extensive low !!!!!!!
(use-package eldoc
  :commands (eldoc-on eldoc-off)
  :config
  (defun eldoc-on ()
    (interactive)
    (eldoc-mode 1))
  (defun eldoc-off ()
    (interactive)
    (eldoc-mode 0))

  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t))

(defun rice-wine-lisp-func ()
  "common features of all lisp mode"
  (rice-wine-prog-func)
  (paredit-on)
  (eldoc-on)
  (yas-on))

(use-package lisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("\\.lisp\\'" . lisp-mode))
  :config
  (defun rice-wine-emacs-lisp-func ()
    (rice-wine-lisp-func)
    (setup-company-mode '(company-elisp)))

  (defun rice-wine-common-lisp-func ()
    (rice-wine-lisp-func)
    (slime-mode))

  (use-package company-elisp)
  (add-hook 'emacs-lisp-mode-hook 'rice-wine-emacs-lisp-func)
  (add-hook 'lisp-mode-hook 'rice-wine-common-lisp-func))


(use-package slime
  :disabled t ;; configuration is not complete
  :commands (slime slime-mode)
  :init
  (require 'slime-autoloads)
  (setq slime-contribs
        '(slime-fancy
          ;; slime-company
          slime-asdf
          slime-autodoc
          ))

  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-net-coding-system 'utf-8-unix)

  (defun slime-repl-set-key ()
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
    (define-key slime-repl-mode-map (kbd "RET") 'slime-repl-newline-and-indent)
    (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-newline-and-indent)
    (define-key slime-repl-mode-map (kbd "C-j") 'slime-repl-return))

  (defun rice-wine-slime-repl-func ()
    (rice-wine-lisp-func)
    (slime-repl-set-key))

  (add-hook 'slime-repl-mode-hook 'rice-wine-slime-repl-func))


(use-package racket-mode
  :mode (("\\.rkt\\'" . racket-mode))
  :disabled t
  :init
  (use-package s
    :defer t)
  :config
  (defun rice-wine-racket-func ()
    (rice-wine-lisp-func)
    (company-mode 1))
  
  (add-hook 'racket-mode-hook 'rice-wine-racket-func))

(use-package scheme
  :mode (("\\.rkt\\'" . scheme-mode)
         ("\\.scm\\'" . scheme-mode))
  :config
  (use-package geiser)
  ;; I don't know when and how the company-mode is turned on. 
  (defun rice-wine-geiser-func ()
    (rice-wine-prog-func)
    (paredit-on)
    (yas-on)
    (company-mode 1))
  
  (add-hook 'scheme-mode-hook 'rice-wine-geiser-func))

;;------------------------------------------------------------------------------
;; coq
;;------------------------------------------------------------------------------
(use-package rw-frame-lib)
(defun rw/pg-show-goals-and-responds-in-other-frame ()
  "show buffer *goals* and *responds* in other frame.
   1. if there is frame in other monitor exists, then switch to that
      frame, rearrange it to show  *goals* and *responds* horizontally
   2. if there is only one frame, then create one, and
      perform same action as 1"
  (interactive)
  (delete-other-windows) ;; delete auto generate layout
  (let ((cframe (selected-frame))
        (xframe (or (rw-select-frame-in-other-monitor)
                    (make-frame))))
    (select-frame xframe)
    ;; now we in new frame
    (switch-to-buffer "*goals*")
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer "*response*")
    (other-window 1)
    (select-frame cframe)))

(evil-leader/set-key
  "cl" 'rw/pg-show-goals-and-responds-in-other-frame)

(use-package proof-site
  :load-path (lambda ()
               (expand-file-name "PG/generic"
                                 rice-wine-package-dir))
  :mode ("\\.v\\'" . coq-mode)
  :config
  (setq
   proof-splash-enable nil
   coq-indent-semicolon-tactical 0
   coq-match-indent 4
   coq-one-command-per-line t
   proof-auto-raise-buffers nil ;; prevent closing the other frame when it only show *goals* and *responds*
   proof-multiple-frames-enable nil ;; this feature is buggy...
   proof-keep-response-history nil
   proof-next-command-insert-space t)

  (defun pg-debug-on ()
    (interactive)
    (setq proof-general-debug t))

  (defun pg-debug-off ()
    (interactive)
    (setq proof-general-debug nil))

  (use-package company-coq
    :commands (company-coq-mode company-coq-initialize)
    :init
    (defun company-coq-on ()
      (interactive)
      (company-coq-initialize))
    (defun company-coq-off ()
      (interacitve)
      (company-coq-mode 0))

    :config
    (setq company-coq-disabled-features
          '(snippets
            outline
            code-folding
            company-defaults
            ;;refman-ltac-abbrevs-backend
            ;;refman-tactic-abbrevs-backend
            ;;refman-vernac-abbrevs-backend
            refman-scope-abbrevs-backend
            pg-backend
            dynamic-symbols-backend
            obsolete-settings))
    (setq company-coq-prettify-symbols-alist
          '(("|-" . 8866)
            ("->" . 8594)
            ("=>" . 8658)
            ("fun" . 955)
            ("forall" . 8704)
            ("exists" . 8707)
            ("/\\" . 8743)
            ("\\/" . 8744)
            ("~" . 172)
            ("+-" . 177)
            (">->" . 8611))))

  (defun coq-mode-func ()
    (rice-wine-prog-func)
    (yas-on)
    (company-coq-on)
    (smartparens-on))

  (add-hook 'coq-mode-hook 'coq-mode-func)
  (use-package rw-coq-lib
    :config
    (evil-leader/set-key
      "ap" 'lzh/coq-trans)))

;;------------------------------------------------------------------------------
;; ruby
;;------------------------------------------------------------------------------
(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :config

  (setq ruby-use-encoding-map nil)

  (use-package inf-ruby
    :config
    (defun rw-ruby-run-file ()
      (interactive)
      (save-current-buffer
        (unless inf-ruby-buffer
          (run-ruby)))
      (ruby-load-file (buffer-file-name))
      (ruby-switch-to-inf t))
    
    (define-key inf-ruby-minor-mode-map
      (kbd "C-c C-l") 'rw-ruby-run-file))

  (use-package robe)

  (defun rice-wine-ruby-func ()
    (rice-wine-prog-func)
    (yas-on)
    (robe-mode)
    (smartparens-on)
    ;; (setup-company-mode '(company-robe))
    )

  (dolist (hook '(ruby-mode-hook inf-ruby-mode-hook))
    (add-hook hook 'rice-wine-ruby-func))
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;;------------------------------------------------------------------------------
;; c
;;------------------------------------------------------------------------------
;; (require 'init-c)


;;------------------------------------------------------------------------------
;; sh
;;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))


(provide 'init-programming)
