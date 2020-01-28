;; -*- coding: utf-8 -*-
(defconst emacs-start-time (current-time))

;;------------------------------------------------------------------------------
;; Define global variables and set global load-path 
;;------------------------------------------------------------------------------
(defvar rice-wine-dir (file-name-directory load-file-name)
  "top directory of configuration")

;; 修改emacs默认的directory到自定义的目录
(setq user-emacs-directory rice-wine-dir)

(defvar rice-wine-lisp-dir (expand-file-name "lisp" rice-wine-dir)
  "configurations of packages")

(defvar rice-wine-package-dir
  (expand-file-name "site-lisp" rice-wine-dir)
  "local packages")

(defvar rice-wine-git-package-dir
  (expand-file-name "git-lisp" rice-wine-dir)
  "packages from git, which have higher priority than pakages in `rice-wine-package-dir'")

(defvar rice-wine-package-temp-dir
  (expand-file-name "temp" rice-wine-dir)
  "try packages from elpa and melpa")

(defvar rice-wine-lib-dir
  (expand-file-name "lib" rice-wine-dir)
  "library packages, mostly for emacs-lisp programming")

;; (defvar rice-wine-configure-file 
;;   (expand-file-name "README.org" rice-wine-dir)
;;   "Store the path of \"README.org\"")

(defvar sys/win32p (eq system-type 'windows-nt))

;; set load-path, include all packages and init files
(defun rw-add-to-load-path (dir)
  "add DIR to the head of load-path"
  (interactive "DDir:")
  (add-to-list 'load-path dir))

(defun rw-add-subdirs-to-load-path (dir)
  "add all subdirs of DIR to load-path, which begin with a digital, letter or '+'."
  (let ((dir-files (directory-files dir t "^[0-9A-Za-z+].*")))
    (dolist (file dir-files)
      (when (file-directory-p file)
        (rw-add-to-load-path file)))))

(defun rw-add-dir-and-subdirs-to-load-path (dir)
  "add DIR and all subdirs of DIR to load-path, which begin with a digital or letter."
  (interactive "DDir:")
  (rw-add-to-load-path dir)
  (rw-add-subdirs-to-load-path dir))

(let ((dirs (list
             ;; init file for packages
             rice-wine-lisp-dir
             ;; packages from elpa
             rice-wine-package-temp-dir
             ;; local packages
             rice-wine-package-dir
             ;; I think useless
             rice-wine-git-package-dir
             ;; library writted by myself
             rice-wine-lib-dir)))
  (mapc #'rw-add-dir-and-subdirs-to-load-path dirs))

;;------------------------------------------------------------------------------
;; other init settings which should set before any functional packages 
;;------------------------------------------------------------------------------
(require 'pre-init)

;;------------------------------------------------------------------------------
;; various extensible functional packages which I prefer called "layer"
;;------------------------------------------------------------------------------
(require 'layers)

;;------------------------------------------------------------------------------
;; Post initialization: show the time finishing configuration
;;------------------------------------------------------------------------------
(when window-system
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
