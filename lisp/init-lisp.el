;;; coding environment of lisp

(add-hook 'rice-wine-lisp-hook 'run-rice-wine-prog-hook)

;; paredit
(require 'paredit)
(defun turn-on-paredit ()
  (paredit-mode +1)
  (define-key paredit-mode-map [remap kill-line] 'paredit-kill)
  (define-key paredit-mode-map (kbd "C-j") nil)
  (local-set-key (kbd "C-j") 'eval-print-last-sexp)
  (local-set-key (kbd "RET") 'paredit-newline)
  (define-key paredit-mode-map [remap bachward-kill-sentence] nil))

(add-hook 'rice-wine-lisp-hook 'turn-on-paredit)

;; eldoc mode make coq-mode extensive low !!!!!!!
(require 'eldoc)
(setq eldoc-idle-delay 0.2)
(setq eldoc-echo-area-use-multiline-p t)
(defun turn-on-eldoc ()
  (turn-on-eldoc-mode))
(add-hook 'rice-wine-lisp-hook 'turn-on-eldoc)

(add-hook 'rice-wine-lisp-hook 'turn-on-yas-mode)
(add-hook 'rice-wine-lisp-hook 'turn-on-show-trailing-whitspace)

(defun run-rice-wine-lisp-hook ()
  (run-hooks 'rice-wine-lisp-hook))

;; specific lisp dialects
(require 'init-emacs-lisp)
(require 'init-common-lisp)

;; end of init-lisp
(provide 'init-lisp)
