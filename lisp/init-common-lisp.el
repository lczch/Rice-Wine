(add-hook 'lisp-mode-hook 'run-rice-wine-lisp-hook)

(add-hook 'lisp-mode-hook
          '(lambda ()
             (slime-mode)))

(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-company))

;; TODO: intelligent <return>, if position at the end of block, run slime-repl-return,
;; else, slime-repl-newline-and-indent.
(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (run-rice-wine-lisp-hook)
             (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
             (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)
             (setq show-trailing-whitespace nil)
             ;;(define-key slime-repl-mode-map (kbd "C-j") 'slime-repl-return)
             (define-key slime-repl-mode-map (kbd "RET") 'slime-repl-newline-and-indent)
             (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-newline-and-indent)
             ))

(provide 'init-common-lisp)
