
(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode))

  :config
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  (use-package xcscope)
  
  (defun rice-wine-c-func ()
    (rice-wine-prog-func)
    (yas-on)
    (smartparens-on)
    (cscope-minor-mode))

  (add-hook 'c-mode-common-hook 'rice-wine-c-func)
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
