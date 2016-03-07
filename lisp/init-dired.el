(require 'stripe-buffer)
(autoload 'turn-on-stripe-buffer-mode "stripe-buffer" "" nil)
(autoload 'turn-on-stripe-table-mode "stripe-buffer" "" nil)
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

(provide 'init-dired)
