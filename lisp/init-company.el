(require 'company)

;; press SPACE will accept the highlighted candidate and insert a space
;; `M-x describe-variable company-auto-complete-chars` for details
;; That's BAD idea.
(setq company-auto-complete nil)

;; turn off "disallow non-matching input"
(setq company-require-match nil)

;; whether to downcase the returned candidates? No
(setq company-dabbrev-downcase nil)

;; ignore case when collecting complete candidates? Maybe I want to try.
;; nil is no, t is yes.
(setq company-dabbrev-ignore-case t)

;; show number of first ten candidates
(setq company-show-numbers t)

;; If I actually get the point, this variable `company-begin-commands` controls
;; what commands of emacs can triger the starting of company.
;; `self-insert-command` means typing IO.
(setq company-begin-commands '(self-insert-command))

;; time from company-begin-commands to start company
(setq company-idle-delay 0.2)

;; use company-statistics to arrange the order of candidates, show more probably selected one to the first
(require 'company-statistics)

(defun setup-company-mode (backends)
  "turn-on company-mode, then make variable company-backends to buffer local, and set it to BACKENDS"
  (company-mode 1)
  (company-statistics-mode)
  (make-local-variable 'company-backends)
  (setq company-backends backends))

;;; useful company-backend
;;  company-c-headers
;;  company-elisp
;;  company-bbdb ;; BBDB stands for The Insidious Big Brother Database – an address book that you can hook into your mail- and newsreader, sync with your mobile device, etc.
;;  company-nxml
;;  company-css
;;  company-eclim
;;  company-semantic ;; completion backend using CEDET Semantic
;;  company-clang
;;  company-xcode
;;  company-cmake
;;  company-capf
;;  (company-dabbrev-code company-gtags company-etags company-keywords)
;;  company-oddmuse
;;  company-files
;;  company-dabbrev ;; this is very useful!


(provide 'init-company)
