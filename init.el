;; -*- coding: utf-8 -*-

;;; Work with Emacs 24.3,
;;; implement for my personal specifications

;;; main directory
(setq rice-wine-dir (expand-file-name "~/rice-wine"))
(add-to-list 'load-path (expand-file-name "lisp" rice-wine-dir))

;;; disable auto-load of packages, I prefer require them manually
(setq package-enable-at-startup nil)

;;; add load path
;; Some packages is in elpa/, get through packages, relying on Network is terrible
;; Some packages is in site-lisp/, manually added
;; configuration files (name start with init-) in lisp/

(require 'cl) ; Compatibility aliases for the old CL library Built-in
(require 'cl-lib) ; Common Lisp extensions for Emacs, Built-in

(require 'init-rice-wine-functions) ; some useful elisp functions at top level

;;; add needed dirs to load-path
(setq rice-wine-package-dir (expand-file-name "site-lisp" rice-wine-dir))
(rice-wine/add-subdirs-to-load-path rice-wine-package-dir)

;;; extend emacs with vim key binding styles
;; This method prevent large damage on my fingers,
;; especially for little finger!
;; main package: evil
(require 'init-evil)

;;; interactively do things with buffers and files
;; main package: ido (built-in)
;; sub package: ido-ubiquitous (enable ido-style completion everywhere)
;; sub package: flx-ido (a more powerful alternative to ido-mode's built-in flex matching)
;; TODO test ido-ubiquitous and flx-ido, I really need them?
(require 'init-ido)

;;; expand-region: increase selected region by semantic units
(require 'init-expand-region)

;;; color theme
(require 'init-color-theme)

;;; TODO
(require 'init-smex) ;; M-x interface with Ido-style fuzzy matching

;;; org-mode
;; TODO org-mime?
(require 'init-org)

;;; yet another snippet
;; main package: yasnippet
(require 'init-yasnippet)

;;; Modular text completion framework: Company
;; Auto completion is necessary feature to Emacs,
;; I think the organization of backends should be more carefully to avoid conflicts
(require 'init-company)

;;; TODO test what hippie-expand do
;; expand text trying various ways to find its expansion (built-in)
(require 'init-hippie-expand)


;;; support of programming language
(require 'init-programming)

;;; Miscellaneous configurations
;; features in this file are all simple and local, that
;; can easily turn on/off and not affect any other part.
(require 'init-misc)







