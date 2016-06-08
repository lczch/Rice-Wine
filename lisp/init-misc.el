;;; ELPA
;; require package from network: I only use it to get a package from network,
;; and move it to site-lisp/, or get the description of a package.
(require 'init-elpa)

;;; configure the appearance of emacs
(require 'init-gui-frame)

;;; functions and configuration about editing text
(require 'init-editing)

;;; functions and manipulation about windows, from purcell's emacs.d
(require 'init-windows)

;;; custom-file and backup-directory
(let ((my-custom-file (expand-file-name "custom.el" rice-wine-dir))
      (my-backup-dir (expand-file-name "backups" rice-wine-dir)))
  (setq custom-file my-custom-file)
  (setq backup-directory-alist `(("." . ,my-backup-dir))))

;;; emacs sessions
(require 'init-sessions)

;;; info mode
(defun info-mode ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

;;; frame-hooks
(require 'init-frame-hooks)

;;; fonts
(require 'init-fonts)

;;; linum mode on
(require 'init-linum-mode)

;;; locales
(require 'init-locales)

;;; markdown-mode
(require 'init-markdown)

(fset 'yes-or-no-p 'y-or-n-p)
(setq history-delete-duplicates t)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; increase and decrease font size in GUI emacs
(when (display-graphic-p)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease))

;; system command
;; (defadvice async-shell-command (before uniqify-running-shell-command activate)
;;   (let ((buf (get-buffer "*Async Shell Command*")))
;;     (if buf
;;         (let ((proc (get-buffer-process buf)))
;;           (if (and proc (eq 'run (process-status proc)))
;;               (with-current-buffer buf
;;                 (rename-uniquely)))))))

(evil-leader/set-key
  "sc" 'async-shell-command)

;; server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (message "success start server")))

;; sh
(require 'init-sh)

;; writting
(require 'init-writting)

;; w3m
(require 'init-emacs-w3m)

;;; TODO
(require 'init-dired) ;; only affect dired-mode
(require 'init-ibuffer) ;; only affect ibuffer-mode

(use-package evil-nerd-commenter
  :config
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    ;; "cc" 'evilnc-copy-and-comment-lines
    ;; "cp" 'evilnc-comment-or-uncomment-paragraphs
    ;; "cr" 'comment-or-uncomment-region
    ;; "cv" 'evilnc-toggle-invert-comment-line-by-line
    ;; "\\" 'evilnc-comment-operator       ; if you prefer backslash key
    ))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; dash keyword hylight

(use-package dash
  :defer t
  :config
  (dash-enable-font-lock))

(provide 'init-misc)
