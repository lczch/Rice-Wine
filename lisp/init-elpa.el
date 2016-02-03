(require 'package)

;;------------------------------------------------------------------------------
;; Standard package repositories
;;------------------------------------------------------------------------------

;; We include the org repository for completeness, but don't use it.
;; Lock org-mode temporarily:
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;; uncomment below line if you need use GNU ELPA
                         ;; ("gnu" . "http://elpa.gnu.org/packages/")
                         ))

(setq rice-wine-package-temp-dir
      (expand-file-name "temp" rice-wine-dir))

(setq package-user-dir rice-wine-package-temp-dir)

(provide 'init-elpa)
