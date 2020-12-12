(use-package stripe-buffer
  :commands (turn-on-stripe-buffer-mode turn-on-stripe-table-mode))

(use-package dired-x)

(use-package dired
  :config 
  (setq dired-recursive-deletes 'always)
  )

(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

(provide 'init-dired)
