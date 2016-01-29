(require 'company)

;; press SPACE will accept the highlighted candidate and insert a space
;; `M-x describe-variable company-auto-complete-chars` for details
;; That's BAD idea.
(setq company-auto-complete nil)

;; disallow non-matching input?
(setq company-require-match nil)




(provide 'init-company)