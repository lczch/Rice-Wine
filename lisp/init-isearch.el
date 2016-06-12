;; Use regex to search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package visual-regexp
  :commands (vr/query-replace)
  :init 
  (evil-leader/set-key
    "rr" 'vr/query-replace
    "vm" 'vr/mc-mark))

(provide 'init-isearch)
