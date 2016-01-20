;;; require packages
(require 'expand-region)

;;; bind keys by evil mode
(evil-leader/set-key
  "xx" 'er/expand-region)

(setq expand-region-contract-fast-key "z")
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)

(provide 'init-expand-region)
