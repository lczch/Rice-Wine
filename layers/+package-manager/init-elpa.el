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
                           ("melpa-stable" . "http://1.15.88.122/stable-melpa/")
                           ("gnu"   . "http://1.15.88.122/gnu/")
                           ("melpa" . "http://1.15.88.122/melpa/")
                           ;; ("melpa" . "https://melpa.org/packages/")
                           ;; ("org" . "https://orgmode.org/elpa/")
                           ))
  
  (setq package-user-dir rice-wine-package-temp-dir))

;; set NO-ACTIVATE of the function to t, don't activate packages, but prepare for downloading packages
(package-initialize t)


(provide 'init-elpa)
