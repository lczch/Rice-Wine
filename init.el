;; -*- coding: utf-8 -*-

;; Work with Emacs 24.5,
;; implement for my personal specifications

;;------------------------------------------------------------------------------
;; prepare work: set working directory and load-path
;;------------------------------------------------------------------------------
(setq rice-wine-dir (expand-file-name "~/rice-wine"))
(add-to-list 'load-path (expand-file-name "lisp" rice-wine-dir))

;; disable auto-load of packages, I prefer require them manually
(setq package-enable-at-startup nil)

;; load base functionalities
(require 'init-utils)

;; add needed dirs to load-path
(setq rice-wine-package-dir (expand-file-name "site-lisp" rice-wine-dir))
(rice-wine/add-subdirs-to-load-path rice-wine-package-dir)

;; use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;------------------------------------------------------------------------------
;; evil: extend emacs with vim key binding styles
;;------------------------------------------------------------------------------
(require 'init-evil)

;;------------------------------------------------------------------------------
;; ido: interactively do things with buffers and files
;;------------------------------------------------------------------------------
(require 'init-ido)

;;------------------------------------------------------------------------------
;; company: text completion
;;------------------------------------------------------------------------------
(require 'init-company)

;;------------------------------------------------------------------------------
;; org-mode: wonderful text organizer
;;------------------------------------------------------------------------------
(require 'init-org)

;;------------------------------------------------------------------------------
;; yasnippet: text template unfolding functionality
;;------------------------------------------------------------------------------
(require 'init-yasnippet)

;;------------------------------------------------------------------------------
;; support for programming languages
;;------------------------------------------------------------------------------
(require 'init-programming)

;;------------------------------------------------------------------------------
;; Miscellaneous configurations
;;------------------------------------------------------------------------------
(require 'init-misc)







