
(add-hook 'lisp-mode-hook 'run-rice-wine-lisp-hook)

(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-company))

(provide 'init-common-lisp)