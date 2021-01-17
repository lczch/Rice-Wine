;;------------------------------------------------------------------------------
;; elpa
;;------------------------------------------------------------------------------
;; require package from network: I only use it to get a package from network,
;; and move it to site-lisp/, or get the description of a package.
(use-package package
  :config
  ;; use mirror in China
  (setq package-archives `(
                           ;; ("site-lisp" . ,rice-wine-package-dir)
                           ;; ("straight-repo" . ,rw-straight-repos-dir)
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")
                           ("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ;; ("melpa" . "https://melpa.org/packages/")
                           ;; ("org" . "https://orgmode.org/elpa/")
                           ))
  
  (setq package-user-dir rice-wine-package-temp-dir))

;; set NO-ACTIVATE of the function to t, don't activate packages, but prepare for downloading packages
(package-initialize t)


(provide 'init-elpa)
