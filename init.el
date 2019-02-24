;; -*- coding: utf-8 -*-

;; Work with Emacs 24.5,
;; implement for my personal specifications

(defconst emacs-start-time (current-time))

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
(setq package-enable-at-startup nil)

(defun rw-add-to-load-path (dir)
  "add DIR to the head of load-path"
  (add-to-list 'load-path dir))

(defun rw-add-subdirs-to-load-path (dir)
  "add all subdirs of DIR to load-path, which begin with a digital or letter."
  (let ((dir-files (directory-files dir t "^[0-9A-Za-z].*")))
    (dolist (file dir-files)
      (when (file-directory-p file)
        (rw-add-to-load-path file)))))

(defun rw-add-dir-and-subdirs-to-load-path (dir)
  "add DIR and all subdirs of DIR to load-path, which begin with a digital or letter."
  (interactive "DDir:")
  (rw-add-to-load-path dir)
  (rw-add-subdirs-to-load-path dir))

;; add needed dirs to load-path
(defvar rice-wine-dir (file-name-directory load-file-name)
  "top directory of configuration")

(defvar rice-wine-lisp-dir (expand-file-name "lisp" rice-wine-dir)
  "configurations of packages")

(defvar rice-wine-package-dir
  (expand-file-name "site-lisp" rice-wine-dir)
  "local packages")

(defvar rice-wine-git-package-dir
  (expand-file-name "new" rice-wine-dir)
  "packages from git, which have higher priority than pakages in `rice-wine-package-dir'")

(defvar rice-wine-lib-dir
  (expand-file-name "lib" rice-wine-dir)
  "library packages, mostly for elisp programming")

;; (defvar rice-wine-app-dir
;;   (expand-file-name "app" rice-wine-dir)
;;   "Some apps writing in elisp.")

(defun rw-add-all-packages-to-load-path ()
  "Add directories in `rice-wine-lib-dir', `rice-wine-git-package-dir' and `rice-wine-package-dir' in `load-path', in which they have the same order."
  (interactive)
  (let ((dirs (list
               rice-wine-package-dir
               ;; rice-wine-git-package-dir
               rice-wine-lib-dir)))
    (mapc #'rw-add-dir-and-subdirs-to-load-path dirs)))

(defun rw-configure-load-path ()
  "Configuring load path for rice-wine emacs"
  (interactive)
  ;; top dir
  (rw-add-to-load-path rice-wine-dir)
  ;; package configuration dir
  (rw-add-dir-and-subdirs-to-load-path rice-wine-lisp-dir)
  ;; package dir
  (rw-add-all-packages-to-load-path)
  )

(rw-configure-load-path)

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
(use-package rw-misc-lib)


;;------------------------------------------------------------------------------
;; start server: if a emacs starts with server, it must be the main emacs!
;;------------------------------------------------------------------------------
(defvar rw-main-emacs-p nil
  "Whether this emacs is the main emacs?")

(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (setq rw-main-emacs-p t)
    (message "rw: success start server!")))

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
(use-package init-minibuff)
(use-package init-windows)


(use-package init-evil)
(use-package init-dired)
(use-package init-ibuffer)


(use-package init-ido)
(use-package init-company)
(use-package org
  :init
  (rw-add-to-load-path (expand-file-name "org-mode/lisp" rice-wine-git-package-dir))
  (rw-add-to-load-path (expand-file-name "org-mode/contrib/lisp" rice-wine-git-package-dir))
  :mode (("\\.org\\'" . org-mode))
  :commands (org-mode)
  :config
  (use-package init-org)
  )


(use-package init-yasnippet)

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

;; TODO: may switch to gtags?
(use-package init-xcscope)

(use-package init-clipboard)

(use-package which-key
  :config
  (which-key-mode 1))

;; TODO: I use this seldom.
(use-package init-emacs-w3m)

;; TODO: I use this seldom.
(use-package init-profiler)

;;------------------------------------------------------------------------------
;; about programming
;;------------------------------------------------------------------------------
(use-package init-programming)
(use-package init-markdown)

(use-package init-latex)

(use-package init-haskell-mode)

;; (use-package tex-mode
;;   :init
;;   (add-hook 'latex-mode-hook 'smartparens-mode)
;;   (add-hook 'latex-mode-hook 'rainbow-delimiters-mode))

;;------------------------------------------------------------------------------
;; misc configurations
;;------------------------------------------------------------------------------
;; debug on
(global-set-key (kbd "<f12>") 'toggle-debug-on-error)

(evil-leader/set-key
  "xh" 'mark-whole-buffer
  "do" 'rw-display-current-buffer-other-frame
  "eb" 'eval-buffer
  "rb" 'revert-buffer) 

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
(setq auto-save-interval 50)
(let ((my-custom-file (expand-file-name "custom.el" rice-wine-dir))
      (my-backup-dir (expand-file-name "backups" rice-wine-dir)))
  (setq custom-file my-custom-file)
  (setq backup-directory-alist `(("." . ,my-backup-dir))))

;; about Semantic
(setq semanticdb-default-save-directory nil)

(global-set-key (kbd "<f5>")
                #'(lambda ()
                    (interactive)
                    (semantic-grammar-create-package)
                    (eval-buffer)))

(global-set-key (kbd "<f6>")
                #'(lambda ()
                    (interactive)
                    (revert-buffer nil t)
                    (bovinate)))

;;------------------------------------------------------------------------------
;; restore desktop
;;------------------------------------------------------------------------------
;; (when rw-main-emacs-p
;;   (use-package init-desktop))

;;------------------------------------------------------------------------------
;; printer: we need to install "xpp" through os package manager
;;------------------------------------------------------------------------------
(setq lpr-command "xpp")

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



