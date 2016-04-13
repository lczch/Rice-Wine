;;; coding environment of lisp
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
  :commands (slime slime-mode)
  :init
  (require 'slime-autoloads)
  (setq slime-contribs
        '(slime-fancy
          slime-company
          slime-asdf
          slime-autodoc
          ))

  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-net-coding-system 'utf-8-unix)

  (defun slime-repl-set-key ()
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
    (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)
    (define-key slime-repl-mode-map (kbd "RET") 'slime-repl-newline-and-indent)
    (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-newline-and-indent))

  (defun rice-wine-slime-repl-func ()
    (rice-wine-lisp-func)
    (slime-repl-set-key))

  (add-hook 'slime-repl-mode-hook 'rice-wine-slime-repl-func))

;; end of init-lisp
(provide 'init-lisp)
