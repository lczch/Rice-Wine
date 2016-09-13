
(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :config

  (setq ruby-use-encoding-map nil)

  (use-package robe)

  (use-package inf-ruby
    :config
    (setq inf-ruby-default-implementation "pry")
    
    ;; (defun rw-ruby-run-file ()
    ;;   (interactive)
    ;;   (save-curret-buffer
    ;;     (unless inf-ruby-buffer
    ;;       (run-ruby)))
    ;;   (ruby-load-file (buffer-file-name))
    ;;   (ruby-switch-to-inf t))

    (defun ruby-send-current-buffer ()
      "Send current buffer to repl. If there is no repl, run `inf-ruby' and `robe-start'."
      (interactive)
      (save-current-buffer
        (unless inf-ruby-buffer
          (inf-ruby inf-ruby-default-implementation)
          (robe-start)))

      (ruby-send-buffer)
      (ruby-switch-to-inf t))
    
    (define-key inf-ruby-minor-mode-map
      (kbd "C-c C-l") 'ruby-send-current-buffer))


  (defvar ruby-company-backends
    '(;; company-capf
      company-robe
      company-files 
      ))
  
  (defun rice-wine-ruby-func ()
    (rice-wine-prog-func)
    (yas-on)
    (robe-mode)
    (setup-company-mode ruby-company-backends)
    )

  (dolist (hook '(ruby-mode-hook inf-ruby-mode-hook))
    (add-hook hook 'rice-wine-ruby-func))
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(provide 'init-ruby)
