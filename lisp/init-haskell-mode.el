
(setq haskell-mode-dir
      (expand-file-name "haskell-mode/" rice-wine-git-package-dir ))

(rw-add-to-load-path haskell-mode-dir)

(require 'haskell-mode-autoloads)

(add-to-list 'Info-default-directory-list haskell-mode-dir)

(use-package company-dabbrev)

(defun rice-wine-haskell-func ()
  (rice-wine-prog-func)
  (yas-on)
  (setup-company-mode '(company-capf company-dabbrev-code))
  )

(add-hook 'haskell-mode-hook 'rice-wine-haskell-func)

(provide 'init-haskell-mode)
