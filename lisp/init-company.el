(use-package company
  :config
  (setq company-auto-complete nil)
  (setq company-require-match nil)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  ;; If I actually get the point, this variable `company-begin-commands` controls
  ;; what commands of emacs can triger the starting of company.
  ;; `self-insert-command` means typing IO.
  ;; (setq company-begin-commands '(self-insert-command))
  (setq company-idle-delay 0.1)

  (use-package company-statistics
    :commands (company-statistics-mode))
  (use-package company-elisp
    :commands (company-elisp))
  (use-package company-capf
    :commands (company-capf))
  (use-package company-files
    :commands (company-files))
  (use-package company-dabbrev
    :commands (company-dabbrev))
  (use-package company-math
    :commands (company-math-symbols-unicode))
  
  ;; use company-statistics to arrange the order of candidates, show more probably selected one to the first
  (defun setup-company-mode (backends)
    "turn-on company-mode, then make variable company-backends to buffer local, and set it to BACKENDS.
     Example: for elisp, (setup-company-mode '(company-elisp))"
    (company-mode 1)
    (company-statistics-mode)
    (make-local-variable 'company-backends)
    (setq company-backends backends))
  )

;;; useful company-backend
;;  company-c-headers
;;  company-elisp
;;  company-bbdb ;; BBDB stands for The Insidious Big Brother Database – an address book that you can hook into your mail- and newsreader, sync with your mobile device, etc.
;;  company-nxml
;;  company-css
;;  company-eclim
;;  company-semantic ;; completion backend using CEDET Semantic
;;  company-clang
;;  company-xcode
;;  company-cmake
;;  company-capf
;;  (company-dabbrev-code company-gtags company-etags company-keywords)
;;  company-oddmuse
;;  company-files
;;  company-dabbrev ;; this is very useful!

(provide 'init-company)
