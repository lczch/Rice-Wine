

;; find a tag 
(global-set-key (kbd "C-\\") 'counsel-etags-find-tag-at-point)
(global-set-key (kbd "C-t") 'pop-tag-mark)

;; debug on
(global-set-key (kbd "<f12>") 'toggle-debug-on-error)

;; comment:: replaceed by "ci"
;; (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; switch frame and buffer
(global-set-key (kbd "C-M-<next>") 'rw/switch-to-next-frame-in-same-monitor)
(global-set-key (kbd "C-M-<prior>") 'rw/switch-to-previous-frame-in-same-monitor)

(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)


;; "Delete other windows in frame if any, or restore previous window config."
(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))


;; 如果在emacs是启动了server的时候(我保证全局只有一个server), 那么"C-x C-s"不会杀掉这个emacs,
;; 需要手动执行`kill-emacs', 才会杀掉最后一个emacs.
(global-set-key (kbd "C-x C-c") 'rw-save-buffers-kill-terminal)
;; ---------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------
(global-set-key (kbd "C-x C-;") 'comment-line)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "M-y") 'browse-kill-ring)

;; ;; window
;; (evil-leader/set-key
;;   "x0" 'delete-window
;;   "x1" 'delete-other-windows
;;   "x2" 'split-window-vertically
;;   "x3" 'split-window-horizontally
;;   "xq" 'delete-window
;;   "xa" 'split-window-vertically
;;   "xd" 'split-window-horizontally
;;   "s0" 'delete-window
;;   "s1" 'delete-other-windows
;;   "s2" 'split-window-vertically
;;   "s3" 'split-window-horizontally
;;   "sq" 'delete-window
;;   "sa" 'split-window-vertically
;;   "sd" 'split-window-horizontally
;;   "oo" 'delete-other-windows
;;   ;; interesting 
;;   "xr" 'rotate-windows
;;   "xt" 'toggle-two-split-window
;;   ;; window numbering
;;   "0" 'select-window-0
;;   "1" 'select-window-1
;;   "2" 'select-window-2
;;   "3" 'select-window-3
;;   "4" 'select-window-4
;;   "5" 'select-window-5
;;   "6" 'select-window-6
;;   "7" 'select-window-7
;;   "8" 'select-window-8
;;   "9" 'select-window-9
;;   )

;; ;; find and grep files 
;; (evil-leader/set-key
;;   ;; counsel-etags 
;;   "rt" 'counsel-etags-recent-tag
;;   "ft" 'counsel-etags-find-tag
;;   ;; find-file-in-project
;;   "kk" 'find-file-in-project-by-selected
;;   "jj" 'find-file-in-project-at-point
;;   "tt" 'find-file-in-current-directory-by-selected
;;   ;; grep-dired: use "find", not "fd", slower.  
;;   "gd" 'grep-dired
;;   ;; rg: use "c-c c-o"(`ivy-occur') in the minibuffer can output the result to a new buffer. 
;;   "rg" 'counsel-rg
;;   ;; provide by "rg.el"
;;   "rG" 'rg
;;   )

;; ;; search: `swiper'
;; (evil-leader/set-key
;;   "ss" 'swiper
;;   "sb" 'swiper-all)


;; ;; counsel 
;; (evil-leader/set-key
;;   "rr" 'counsel-recentf
;;   "xm" 'counsel-M-x
;;   "xf" 'counsel-find-file
;;   ;; files
;;   "ff"  'counsel-find-file
;;   "fL"  'counsel-locate
;;   ;; help
;;   "?"   'counsel-descbinds
;;   "hdf" 'counsel-describe-function
;;   "hdv" 'counsel-describe-variable
;;   ;; insert
;;   "iu"  'counsel-unicode-char
;;   ;; jump
;;   ;; register/ring
;;   "ry"  'counsel-yank-pop
;;   ;; jumping
;;   "sj"  'counsel-imenu
;;   ;; themes
;;   "Ts"  'counsel-load-theme
;;   "cf" 'counsel-grep ; grep current buffer
;;   )

;; ;; edit: replace
;; (evil-leader/set-key
;;   "lq" 'vr/query-replace
;;   "xx" 'er/expand-region
;;   ;; "vm" 'vr/mc-mark
;;   )

;; ;; org 
;; (evil-leader/set-key
;;   "oa" 'org-archive-subtree-default
;;   "oci" 'org-clock-in
;;   "oco" 'org-clock-out)

;; ;; test: create a new emacs
;; (evil-leader/set-key
;;   "rwt" 'rw-test-new-config)

;; ;; tricky use: for a specific purpose 
;; ;; (evil-leader/set-key
;; ;;   "ap" 'lzh/coq-trans)

;; ;; improve pg's *goals* and *respons* display
;; ;; coq's layout in multi frames.
;; (evil-leader/set-key
;;   "cl" 'rw/pg-show-goals-and-responds-in-other-frame)

;; ;; buffer 
;; (evil-leader/set-key
;;   "xh" 'mark-whole-buffer
;;   "do" 'rw-display-current-buffer-other-frame
;;   "eb" 'eval-buffer
;;   "rb" 'revert-buffer
;;   )

;; ;; comment a line 
;; (evil-leader/set-key
;;   "ci" 'evilnc-comment-or-uncomment-lines
;;   "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
;;   ;; "cc" 'evilnc-copy-and-comment-lines
;;   ;; "cp" 'evilnc-comment-or-uncomment-paragraphs
;;   ;; "cr" 'comment-or-uncomment-region
;;   ;; "cv" 'evilnc-toggle-invert-comment-line-by-line
;;   ;; "\\" 'evilnc-comment-operator       ; if you prefer backslash key
;;   )

;; ;; magit
;; (evil-leader/set-key
;;   ;; "gb"  'spacemacs/git-blame-micro-state
;;   "gfh" 'magit-log-buffer-file
;;   "gm"  'magit-dispatch-popup
;;   "gs"  'magit-status
;;   "gS"  'magit-stage-file
;;   "gU"  'magit-unstage-file)

;; ;; profiler: like "top" in linux 
;; (evil-leader/set-key
;;   "ps" 'rw-profiler-toggle
;;   "pr" 'profiler-report)

;; ;; save/read desktop (emacs的工作区概念)
;; ;; (evil-leader/set-key
;; ;;   "ds" 'rw-desktop-save
;; ;;   "dr" 'rw-desktop-read)

;; ;; tex
;; (evil-leader/set-key
;;   "tca" 'TeX-command-run-all)

;; ;; common
;; (evil-leader/set-key
;;   "xb" 'ivy-switch-buffer
;;   "xh" 'mark-whole-buffer
;;   "xk" 'kill-buffer
;;   "xs" 'save-buffer
;;   "vf" 'vc-rename-file-and-buffer
;;   ;; "vc" 'vc-copy-file-and-rename-buffer
;;   "dj" 'dired-jump ;; open the dired from current file
;;   "bf" 'beginning-of-defun
;;   "m" 'evil-set-marker
;;   "ef" 'end-of-defun
;;   "eb" 'eval-buffer
;;   "ee" 'pp-eval-last-sexp
;;   ;; "aa" 'copy-to-x-clipboard ; used frequently
;;   ;; "pp" 'paste-from-x-clipboard ; used frequently
;;   )

;; ;; from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; (defun vc-rename-file-and-buffer ()
;;   "Rename the current buffer and file it is visiting."
;;   (interactive)
;;   (let* ((filename (buffer-file-name)))
;;     (cond
;;      ((not (and filename (file-exists-p filename)))
;;       (message "Buffer is not visiting a file!"))
;;      (t
;;       (let* ((new-name (read-file-name "New name: " filename)))
;;         (cond
;;          ((vc-backend filename) (vc-rename-file filename new-name))
;;          (t
;;           (rename-file filename new-name t)
;;           (rename-buffer new-name)
;;           (set-visited-file-name new-name)
;;           (set-buffer-modified-p nil))))))))

;; (use-package key-chord
;;   :config
;;   (key-chord-define-global ",."     "()\C-b")
;;   (key-chord-define-global "cv"     "<>\C-b")
;;   (key-chord-define-global "4r"     "$")
;;   (key-chord-mode 1)
;;   )

(provide 'init-keybindings)
