
(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :config

  (setq ruby-use-encoding-map nil)

  (use-package inf-ruby
    :config
    (defun rw-ruby-run-file ()
      (interactive)
      (save-current-buffer
        (unless inf-ruby-buffer
          (run-ruby)))
      (ruby-load-file (buffer-file-name))
      (ruby-switch-to-inf t))
    
    (define-key inf-ruby-minor-mode-map
      (kbd "C-c C-l") 'rw-ruby-run-file))

  (use-package robe)

  (defun rice-wine-ruby-func ()
    (rice-wine-prog-func)
    (yas-on)
    (robe-mode)
    (setup-company-mode '(company-capf company-robe))
    )

  (dolist (hook '(ruby-mode-hook inf-ruby-mode-hook))
    (add-hook hook 'rice-wine-ruby-func))
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(provide 'init-ruby)
