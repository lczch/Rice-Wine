
(use-package cc-mode
  :init
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode))

  :config
  (setq c-default-style "linux")
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;; company
  (use-package company-clang
    :config
    (setq company-clang-insert-arguments nil))
  
  (use-package company-dabbrev)
  (defvar cc-mode-company-backends
    '(company-clang company-dabbrev))

  ;; eldoc
  (use-package c-eldoc)

  (defun rice-wine-cc-func ()
    "features needed by c-mode"
    (rice-wine-prog-func)
    (yas-on)
    (setq c-basic-offset 8)
    (cscope-minor-mode)
    (setup-company-mode cc-mode-company-backends)
    (c-turn-on-eldoc-mode))
  
  (add-hook 'c-mode-common-hook 'rice-wine-cc-func)
)


;; (require 'company-clang)
;; (require 'company-dabbrev)
;; (defun turn-on-company-mode-for-c ()
;;   (setup-company-mode '((company-clang company-dabbrev))))

;; (require 'c-eldoc)

;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (c-turn-on-eldoc-mode)))

;; (setq company-clang-insert-arguments nil)

;; (add-hook 'c-mode-common-hook 'turn-on-company-mode-for-c)


(provide 'init-cc-mode)
