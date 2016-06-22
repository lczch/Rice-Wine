;; -*- coding: utf-8 -*-

;; Work with Emacs 24.5,
;; implement for my personal specifications

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

;;------------------------------------------------------------------------------
;; prepare work: set working directory and load-path
;;------------------------------------------------------------------------------
(defvar rice-wine-dir (expand-file-name "~/rice-wine")
  "top directory of configuration")

(add-to-list 'load-path (expand-file-name "lisp" rice-wine-dir))

;; disable auto-load of packages, I prefer require them manually
(setq package-enable-at-startup nil)

(defun rw-add-subdirs-to-load-path (dir)
  "add all subdirs of DIR to load-path, using internal function
   normal-top-level-add-subdirs-to-load-path"
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defun rw-add-to-load-path (dir)
  "add DIR to the head of load-path"
  (add-to-list 'load-path dir))

;; add needed dirs to load-path
(defvar rice-wine-package-dir
  (expand-file-name "site-lisp" rice-wine-dir)
  "packages' src directory")

(rw-add-subdirs-to-load-path rice-wine-package-dir)

(defvar rice-wine-git-package-dir
  (expand-file-name "git-lisp" rice-wine-dir)
  "packages from my git")

(rw-add-subdirs-to-load-path rice-wine-git-package-dir)

;;------------------------------------------------------------------------------
;; use-package: base abstraction tool
;;------------------------------------------------------------------------------
(eval-and-compile
  (require 'cl)
  (defvar use-package-verbose t)
  (require 'use-package))
  
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;------------------------------------------------------------------------------
;; useful lib
;;------------------------------------------------------------------------------
(use-package cl)
(use-package cl-lib)

(defvar rice-wine-lib-dir
  (expand-file-name "lib" rice-wine-dir))

(rw-add-to-load-path rice-wine-lib-dir)
(rw-add-subdirs-to-load-path rice-wine-lib-dir)

(use-package dash
  :config
  (dash-enable-font-lock))

(use-package s)
(use-package other-lib)
(use-package rw-frame-lib)

;;------------------------------------------------------------------------------
;; individual package configuration
;;------------------------------------------------------------------------------
;; global key bindings
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (setq evil-leader/leader ","))


(use-package init-elpa)
(use-package init-locales)
;; configure the appearance of emacs
(use-package init-gui-frame)
(use-package init-fonts)
(use-package init-isearch)
(use-package init-minibuff
  :disabled t)
(use-package init-windows)


(use-package init-evil)
(use-package init-desktop)
(use-package init-dired)
(use-package init-ibuffer)


(use-package init-ido)
(use-package init-company)
(use-package init-org)
(use-package init-yasnippet)
;; functions and manipulation about windows, base from purcell's emacs.d

;; Nicer naming of buffers for files with identical names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package init-info-mode
  :mode (("\\.info\\'" . info-mode)))

(use-package visual-regexp
  :commands (vr/query-replace)
  :init 
  (evil-leader/set-key
    "rr" 'vr/query-replace
    ;; "vm" 'vr/mc-mark
    ))

;; expand-region: increase selected region by semantic units
(use-package expand-region
  :config
  (evil-leader/set-key
    "xx" 'er/expand-region)
  
  (setq expand-region-contract-fast-key "z")
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  )

;; save place
(use-package saveplace
  :config
  (setq-default save-place t))

;; Highlight the cursor whenever the window scrolls
;; beacon: need package "seq"
(use-package beacon
  :config
  (beacon-mode 1))

(use-package browse-kill-ring
  :config
  ;; no duplicates
  (setq browse-kill-ring-display-duplicates nil)
  ;; preview is annoying
  (setq browse-kill-ring-show-preview nil)
  (browse-kill-ring-default-keybindings)
  (define-key evil-normal-state-map (kbd "M-y") 'browse-kill-ring)
  ;; hotkeys:
  ;; n/p => next/previous
  ;; s/r => search
  ;; l => filter with regex
  ;; g => update/refresh
  )

;; server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (message "success start server")))

(use-package init-programming)
(use-package init-markdown)
(use-package init-emacs-w3m)
(use-package init-profiler)

;;------------------------------------------------------------------------------
;; misc configurations
;;------------------------------------------------------------------------------
(defun rw-display-current-buffer-other-frame ()
  "display current buffer on other frame"
  (interactive)
  (display-buffer-other-frame (current-buffer)))

(evil-leader/set-key
  "xh" 'mark-whole-buffer
  "do" 'rw-display-current-buffer-other-frame)


(fset 'yes-or-no-p 'y-or-n-p)
(setq history-delete-duplicates t)

;; some basic preferences
(setq-default buffers-menu-max-size 30
              case-fold-search t
              save-interprogram-paste-before-kill t
              indent-tabs-mode nil
              mouse-yank-at-point t
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)

;; custom-file and backup-directory
(let ((my-custom-file (expand-file-name "custom.el" rice-wine-dir))
      (my-backup-dir (expand-file-name "backups" rice-wine-dir)))
  (setq custom-file my-custom-file)
  (setq backup-directory-alist `(("." . ,my-backup-dir))))

;;------------------------------------------------------------------------------
;; misc functions
;;------------------------------------------------------------------------------
(defun rw-test-new-config ()
  (interactive)
  (async-shell-command "emacs --debug"))


;;------------------------------------------------------------------------------
;; Post initialization
;;------------------------------------------------------------------------------
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))




