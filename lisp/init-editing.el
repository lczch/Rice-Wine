;;; isearch
(require 'init-isearch)

;;; expand-region: increase selected region by semantic units
(require 'init-expand-region)

;;; save place
(require 'init-saveplace)

;;; Highlight the cursor whenever the window scrolls
;; beacon: need package "seq"
(require 'beacon)
(beacon-mode 1)

;;; browse-kill-ring
(require 'browse-kill-ring)
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

(require 'init-uniquify) ;; nicer naming buffers for files with identical names

(evil-leader/set-key
  "xh" 'mark-whole-buffer
  "do" 'display-buffer-other-frame)

(global-set-key (kbd "C-c o") 'other-frame)

(provide 'init-editing)
