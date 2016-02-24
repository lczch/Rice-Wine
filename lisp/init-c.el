(setq c-default-style "linux")

(setq c-basic-offset 4)
;; give me NO newline automatically after electric expressions are entered
(setq c-auto-newline nil)

(require 'company-clang)
(require 'company-dabbrev)
(defun turn-on-company-mode-for-c ()
  (setup-company-mode '((company-clang company-dabbrev))))

(require 'c-eldoc)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-turn-on-eldoc-mode)))

(setq company-clang-insert-arguments nil)

(add-hook 'c-mode-common-hook 'run-rice-wine-prog-hook)
(add-hook 'c-mode-common-hook 'turn-on-company-mode-for-c)
(add-hook 'c-mode-common-hook 'turn-on-yas-mode)


(provide 'init-c)
