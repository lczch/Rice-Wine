;;; isearch
;; why (require 'isearch) fail?
;; because isearch is inner of Emacs, and not has (provide 'isearch)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(provide 'init-isearch)
