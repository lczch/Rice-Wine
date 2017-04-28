;; (require 'org)

;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

;; {{ NO spell check for embedded snippets


(use-package htmlize)

(defun org-mode-is-code-snippet ()
  (let (rlt
        (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
        (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
        (old-flag case-fold-search)
        b e)
      (save-excursion
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt t))
    rlt))

;; no spell check for property
(defun org-mode-current-line-is-property ()
  (let (cur-line)
    (setq cur-line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
    ;; (message "cur-line=%s" cur-line)
    (string-match "^[ \t]+:[A-Z]+:[ \t]+" cur-line)))

;; Please note flyspell only use ispell-word
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let ((run-spellcheck ad-return-value))
    (if ad-return-value
      (cond
       ((org-mode-is-code-snippet)
        (setq run-spellcheck nil))
       ((org-mode-current-line-is-property)
        (setq run-spellcheck nil))))
    (setq ad-return-value run-spellcheck)))
;; }}

;; Org v8 change log:
;; @see http://orgmode.org/worg/org-8.0.html

;; {{ export org-mode in Chinese into PDF
;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
;; and you need install texlive-xetex on different platforms
;; To install texlive-xetex:
;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
(setq org-latex-to-pdf-process ;; org v7
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-pdf-process org-latex-to-pdf-process) ;; org v8
;; }}

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
(defun narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((equal major-mode 'org-mode) (org-narrow-to-subtree))
        (t (error "Please select a region to narrow to"))))

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-src-content-indentation 0
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      ;; org v7
      org-export-odt-preferred-output-format "doc"
      ;; org v8
      org-odt-preferred-output-format "doc"
      org-tags-column 80
      ;; org-startup-indented t
      ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
      ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
      org-agenda-inhibit-startup t ;; ~50x speedup
      org-agenda-use-tag-inheritance nil ;; 3-4x speedup
      ;; }}
      )

;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(eval-after-load 'org-clock
  '(progn
     (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
     (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

(eval-after-load 'org
  '(progn
     (setq org-imenu-depth 9)
     (require 'org-clock)
     ;; @see http://irreal.org/blog/1
     (setq org-src-fontify-natively t)))

;; 插入时间时, 显示到分钟
(defun rw-org-time-stamp ()
  (interactive)
  (org-time-stamp t))

(defun org-mode-hook-setup ()
  (setq evil-auto-indent nil)
  ;; org-mode's own flycheck will be loaded
  ;; (enable-flyspell-mode-conditionally)

  ;; but I don't want to auto spell check when typing,
  ;; please comment out `(flyspell-mode -1)` if prefer auto spell check
  ;; (flyspell-mode -1)

  ;; don't spell check double words
  ;; (setq flyspell-check-doublon nil)

  ;; display wrapped lines instead of truncated lines
  (setq truncate-lines nil)
  (setq word-wrap t)
  ;; added by rice-wine
  ;; (rainbow-delimiters-mode)
  (smartparens-mode)
  (yas-minor-mode)
  ;; company
  (setup-company-mode '((company-dabbrev
                         company-math-symbols-unicode)
                        ))

  ;; 插入时间时, 显示到分钟
  (define-key org-mode-map [remap org-time-stamp] 'rw-org-time-stamp)
  )
(add-hook 'org-mode-hook 'org-mode-hook-setup)

(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond ((equal (ad-get-arg 0) '(4))
                'browse-url-generic)
               ((equal (ad-get-arg 0) '(16))
                'choose-browser)
               (t
                (lambda (url &optional new)
                  (w3m-browse-url url t))))))
    ad-do-it))

(defadvice org-publish (around org-publish-advice activate)
  "Stop running major-mode hook when org-publish"
  (let ((old load-user-customized-major-mode-hook))
    (setq load-user-customized-major-mode-hook nil)
    ad-do-it
    (setq load-user-customized-major-mode-hook old)))

;;------------------------------------------------------------------------------
;; `org-entities' contain many useful symbols! C-c C-x \ toggle this feature.
;; use `org-entities-help' to see all symbols.
;;------------------------------------------------------------------------------

(defun org-entities-find-utf (symbol)
  "find a utf-8 symbol in `org-entities'"
  (cl-loop for item in org-entities
           when (listp item)
           do (message "%s" (nth 6 item))
           and when (string= symbol (nth 6 item)) return item   
           finally (return nil)
           ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; {{ org2nikola set up                                    ;;
;; (setq org2nikola-output-root-directory "~/.config/nikola") ;;
;; (setq org2nikola-use-google-code-prettify t)               ;;
;; (setq org2nikola-prettify-unsupported-language             ;;
;;       '(elisp "lisp"                                       ;;
;;               emacs-lisp "lisp"))                          ;;
;; ;; }}                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq org-log-done t                                                                       ;;
;;       org-completion-use-ido t                                                             ;;
;;       org-edit-src-content-indentation 0                                                   ;;
;;       org-edit-timestamp-down-means-later t                                                ;;
;;       org-agenda-start-on-weekday nil                                                      ;;
;;       org-agenda-span 14                                                                   ;;
;;       org-agenda-include-diary t                                                           ;;
;;       org-agenda-window-setup 'current-window                                              ;;
;;       org-fast-tag-selection-single-key 'expert                                            ;;
;;       org-export-kill-product-buffer-when-displayed t                                      ;;
;;       ;; org v7                                                                            ;;
;;       org-export-odt-preferred-output-format "doc"                                         ;;
;;       ;; org v8                                                                            ;;
;;       org-odt-preferred-output-format "doc"                                                ;;
;;       org-tags-column 80                                                                   ;;
;;       ;; org-startup-indented t                                                            ;;
;;       ;; {{ org 8.2.6 has some performance issue. Here is the workaround.                  ;;
;;       ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html ;;
;;       org-agenda-inhibit-startup t ;; ~50x speedup                                         ;;
;;       org-agenda-use-tag-inheritance nil ;; 3-4x speedup                                   ;;
;;       ;; }}                                                                                ;;
;;       )                                                                                    ;;
;;                                                                                            ;;
;; (defun rice-wine-org-mode-hook ()                                                          ;;
;;   (setq evil-auto-indent nil)                                                              ;;
;;   (setq truncate-lines nil)                                                                ;;
;;   (setq word-wrap t)                                                                       ;;
;;   (turn-on-yas-mode))                                                                      ;;
;; (add-hook 'org-mode-hook 'rice-wine-org-mode-hook)                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; allow one character to represent plain ordered list
(setq org-list-allow-alphabetical t)

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)))

;; show syntax highlighting per language native mode in *.org
(setq org-src-fontify-natively t)

;; For languages with significant whitespace like Python, but I don't need it in coq.
;; (setq org-src-preserve-indentation t)

;;------------------------------------------------------------------------------
;; Toggle display of entities as UTF-8 characters
;; or #+STARTUP entitiespretty
;; characters are in `org-entities'
;;------------------------------------------------------------------------------
;; (setq org-pretty-entities t)


;;------------------------------------------------------------------------------
;; setting up capture
;;------------------------------------------------------------------------------
(setq org-default-notes-file (expand-file-name "notes.org" "~/org/task"))
;; using org-capture to add ad hoc thinkings to note.org
(define-key global-map (kbd "C-c c") 'org-capture)

;; place all tasks in task.org, using agenda display them
(add-to-list 'org-agenda-files (expand-file-name "task/task.org" "~/org"))
(define-key global-map (kbd "C-c a") 'org-agenda)

;; using C-c C-w (org-refile) move finished task to the right place
;; using `org-archive-subtree-default' quick move finished task to specific archive files.
;; or use default key binding: C-c C-x C-a
(evil-leader/set-key
  "oa" 'org-archive-subtree-default)

(provide 'init-org)
