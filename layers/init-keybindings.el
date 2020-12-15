

;; find a tag 
(global-set-key (kbd "C-\\") 'counsel-etags-find-tag-at-point)


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
(global-set-key "\C-x1" 'sanityinc/toggle-delete-other-windows)
(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))


;; 如果在emacs是启动了server的时候(我保证全局只有一个server), 那么"C-x C-s"不会杀掉这个emacs,
;; 需要手动执行`kill-emacs', 才会杀掉最后一个emacs.
(global-set-key (kbd "C-x C-c") 'rw-save-buffers-kill-terminal)
;; ---------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------
(evil-leader/set-key
    "rr" 'vr/query-replace
    "xx" 'er/expand-region
    ;; "vm" 'vr/mc-mark
    )

;; buffer 
(evil-leader/set-key
  "xh" 'mark-whole-buffer
  "do" 'rw-display-current-buffer-other-frame
  "eb" 'eval-buffer
  "rb" 'revert-buffer
  )

;; comment a line 
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  ;; "cc" 'evilnc-copy-and-comment-lines
  ;; "cp" 'evilnc-comment-or-uncomment-paragraphs
  ;; "cr" 'comment-or-uncomment-region
  ;; "cv" 'evilnc-toggle-invert-comment-line-by-line
  ;; "\\" 'evilnc-comment-operator       ; if you prefer backslash key
  )

;; magit
(evil-leader/set-key
  ;; "gb"  'spacemacs/git-blame-micro-state
  "gfh" 'magit-log-buffer-file
  "gm"  'magit-dispatch-popup
  "gs"  'magit-status
  "gS"  'magit-stage-file
  "gU"  'magit-unstage-file)

;; profiler: like "top" in linux 
(evil-leader/set-key
  "ps" 'rw-profiler-toggle
  "pr" 'profiler-report)

;; save/read desktop (emacs的工作区概念)
(evil-leader/set-key
  "ds" 'rw-desktop-save
  "dr" 'rw-desktop-read)

;; clipboard: usually not used in windows...
(evil-leader/set-key
  "aa" 'copy-to-x-clipboard
  "zz" 'paste-from-x-clipboard)



(provide 'init-keybindings)
