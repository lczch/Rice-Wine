;; -*- coding: utf-8 -*-

;; Work with Emacs 24.3,
;; implement for my personal specifications

;; main directory
(setq rice-wine-dir (expand-file-name "~/rice-wine"))


;; add load path
; Some packages is in elpa/, get through packages, relying on Network is terrible
; Some packages is in site-lisp/, manually added
; configuration files (name start with init-) in lisp/

(require 'cl) ;; Compatibility aliases for the old CL library Built-in
(require 'cl-lib) ;; Common Lisp extensions for Emacs, Built-in

;; add site-lisp/* to load-path
(let* ((site-lisp-dir (concat rice-wine-dir "site-lisp/"))
       (default-directory site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-utils) ;; some useful elisp functions

;; interactively do things with buffers and files
;; main package: ido (built-in)
;; sub package: ido-ubiquitous (enable ido-style completion everywhere)
;; sub package: flx-ido (a more powerful alternative to ido-mode's built-in flex matching)
;; TODO test ido-ubiquitous and flx-ido, I really need them?
(require 'init-ido)

;; TODO
(require 'init-dired) ;; only affect dired-mode
(require 'init-uniquify) ;; nicer naming buffers for files with identical names
(require 'init-ibuffer) ;; only affect ibuffer-mode

;; TODO
(require 'smex) ;; M-x interface with Ido-style fuzzy matching

;; TODO test what hippie-expand do
;; expand text trying various ways to find its expansion (built-in)
(require 'init-hippie-expand)

;; org-mode
;; TODO org-mime?
(require 'init-org)





