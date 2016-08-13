(use-package xcscope
  :init
  (defun cscope-add-file-suffix (&rest suffixes)
    (dolist (elt suffixes)
      (add-to-list 'cscope-indexer-suffixes elt)))
  
  :commands (cscope-minor-mode)
  :config
  (cscope-add-file-suffix
   "*.[sS]" ; asm
   "*.v" ; coq 
   "*.el" ; elisp
   "*.clj" ; clojure
   )
  (setq cscope-symbol-chars "A-Za-z0-9_")
  )

(provide 'init-xcscope)
