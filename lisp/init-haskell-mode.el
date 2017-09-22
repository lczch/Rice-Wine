;;; information about haskell-mode

;;------------------------------------------------------------------------------
;; `load-path'
;;------------------------------------------------------------------------------
;; We need not to update it manually, unless there are some necessary special subdirectories.
;; The code is always as:
;; (setq haskell-mode-dir
;;       (expand-file-name "haskell-mode/" rice-wine-git-package-dir ))
;; (add-to-list 'load-path haskell-mode-dir)

;;------------------------------------------------------------------------------
;; Configure `company-backends' locally, the `company-dabbrev-code' is always useful.
;;------------------------------------------------------------------------------
;; This code should be placed in hook of major mode.
;; (setup-company-mode '(company-capf company-dabbrev-code))

;;------------------------------------------------------------------------------
;; Configure minor mode
;;------------------------------------------------------------------------------
;; These minor modes together with `company-backends' place in `rice-wine-name-func',
;; this functions will be tangled in major mode hook.
;; (defun rice-wine-haskell-func ()
;;   (rice-wine-prog-func)
;;   (yas-on)
;;   (setup-company-mode '(company-capf company-dabbrev-code))
;;   )

;;------------------------------------------------------------------------------
;; Write configures to the major hook
;;------------------------------------------------------------------------------
;; (add-hook 'haskell-mode-hook 'rice-wine-haskell-func)


(use-package haskell-mode-autoloads
  :init
  (setq rice-wine-haskell-mode-dir
        (expand-file-name "haskell-mode/" rice-wine-git-package-dir ))
  (add-to-list 'Info-default-directory-list rice-wine-haskell-mode-dir)

  :config
  (defun rice-wine-haskell-func ()
    (rice-wine-prog-func)
    (yas-on)
    (setup-company-mode '(company-capf company-dabbrev-code))
    (interactive-haskell-mode)
    )

  (defun rice-wine-haskell-interactive-func ()
    (rice-wine-prog-func)
    (setup-company-mode '(company-capf)))
  
  (add-hook 'haskell-mode-hook 'rice-wine-haskell-func)

  (require 'haskell-interactive-mode)
  (require 'haskell-process)

  (add-hook 'haskell-interactive-mode-hook 'rice-wine-haskell-interactive-func)
  
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
  
  )

(provide 'init-haskell-mode)
