(use-package stripe-buffer
  :commands (turn-on-stripe-buffer-mode turn-on-stripe-table-mode))

(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

(provide 'init-dired)
