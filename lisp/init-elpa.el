;;------------------------------------------------------------------------------
;; elpa
;;------------------------------------------------------------------------------
;; require package from network: I only use it to get a package from network,
;; and move it to site-lisp/, or get the description of a package.
(use-package package
  :config
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ;; uncomment below line if you need use GNU ELPA
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ))
  
  (defvar rice-wine-package-temp-dir
    (expand-file-name "temp" rice-wine-dir)
    "temp dir saving packages from elpa")
  
  (setq package-user-dir rice-wine-package-temp-dir))

(provide 'init-elpa)
