(defun print-load-path ()
  "print load-path to *message*, for debugging"
  (interactive)
  (dolist (elt load-path)
    (princ (format "%s\n" elt))))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

;; five times of default value
(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
(setq gc-cons-threshold best-gc-cons-threshold)
;;------------------------------------------------------------------------------
;; prepare work: set working directory and load-path
;;------------------------------------------------------------------------------
;; Disable auto-load of packages. I prefer requiring them manually.
;; use pacakge `straight` instead 
(setq package-enable-at-startup nil)



;; (print-load-path)


;;------------------------------------------------------------------------------
;; use-package: wonderful organization tool of emacs configuration 
;;------------------------------------------------------------------------------
(eval-and-compile
  (require 'cl)
  (defvar use-package-verbose nil) ;; debug message
  (require 'use-package))

(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;------------------------------------------------------------------------------
;; useful lib
;;------------------------------------------------------------------------------
(use-package cl)
(use-package cl-lib)

(use-package dash
  :config
  (dash-enable-font-lock))

(use-package s)
(use-package f)

(use-package other-lib)
(use-package rw-frame-lib)
(use-package rw-buffer-lib)
(use-package rw-file-lib)
(use-package rw-misc-lib
  :commands (rw-test-new-config))
(use-package general)

;; newer version seq.el for emacs 
;; (use-package my-seq)
(provide 'pre-init)
