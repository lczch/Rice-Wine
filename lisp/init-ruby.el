(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :config

  (setq ruby-use-encoding-map nil)
  (use-package robe
    :init
    (use-package inf-ruby)

    :config
    (defun rice-wine-ruby-func ()
      (rice-wine-prog-func)
      ;; (yas-on)
      (robe-mode)
      (setup-company-mode '(company-robe)))

    (dolist (hook '(ruby-mode-hook inf-ruby-mode-hook))
      (add-hook hook 'rice-wine-ruby-func))
    (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)))


(provide 'init-ruby)
