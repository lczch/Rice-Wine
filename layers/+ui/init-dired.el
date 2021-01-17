(use-package stripe-buffer
  :commands (turn-on-stripe-buffer-mode turn-on-stripe-table-mode))

(use-package dired-x)

(use-package dired
  :config 
  (setq dired-recursive-deletes 'always)
  ;; don't interrupt my leader key!
  (bind-keys
   :map dired-mode-map
   ("SPC" . nil))
  )

(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

(provide 'init-dired)
