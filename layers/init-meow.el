
(use-package meow
  :config
  (defun my-meow-quit ()
    "Quit current window or buffer."
    (interactive)
    (if (> (seq-length (window-list (selected-frame))) 1)
        (kill-buffer)
      (previous-buffer)))

  (defun my-toggle-delete-other-windows ()
    (interactive)
    (cond
     ((minibufferp)
      (keyboard-escape-quit))
     (t
      (sanityinc/toggle-delete-other-windows))))

  (defun my-copy-to-x-clipboard ()
    (interactive)
    (copy-to-x-clipboard)
    (meow--cancel-selection))
  
  (defun meow-setup ()
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . meow-change-save)
     '("x" . meow-delete)
     '("s" . meow-line)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" . meow-keyboard-quit)
     '("G" . goto-line)
     '("h" . meow-head)
     '("H" . meow-head-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("m" . meow-join)
     '("M" . delete-indentation)
     '("d" . meow-kill)
     '("t" . meow-till)
     '("T" . meow-till-expand)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-block-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("q" . previous-buffer)
     '("r" . meow-replace)
     '("R" . meow-replace-save)
     '("n" . meow-search)
     '("N" . meow-pop-search)
     '("l" . meow-tail)
     '("L" . meow-tail-expand)
     '("u" . undo)
     '("v" . meow-visit)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("y" . my-copy-to-x-clipboard)
     '("p" . yank)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection)
     '("&" . meow-query-replace)
     '("%" . meow-query-replace-regexp)
     '("<escape>" . next-buffer))
    
    (meow-leader-define-key
     '(",ss" . swiper)
     '(",kk" . find-file-in-project-by-selected)
     '(",jj" . find-file-in-project-at-point)
     '(",rg" . counsel-rg)
     '(",rr" . vr/query-replace)
     '(",rwt" . rw-test-new-config)
     '(",xh" . mark-whole-buffer)
     '(",do" . rw-display-current-buffer-other-frame)
     '(",eb" . eval-buffer)
     '(",rb" . revert-buffer)
     '(",gs" . magit-status)
     '(",ps" . rw-profiler-toggle)
     '(",pr" . profiler-report)
     '(",aa" . copy-to-x-clipboard)
     '(",zz" . paste-from-x-clipboard)
     '(",ci" . comment-line)
     '("<right>" . next-buffer)
     '("<left>" . previous-buffer)
     ;; '("j" . next-buffer)
     ;; '("k" . previous-buffer)
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . meow-motion-origin-command)
     '("k" . meow-motion-origin-command)
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     )) 
  
  (meow-global-mode 1)
  
  (meow-setup))



;; profiler: like "top" in linux 
;; (evil-leader/set-key
;;   "ps" 'rw-profiler-toggle
;;   "pr" 'profiler-report)

;; ;; save/read desktop (emacs的工作区概念)
;; (evil-leader/set-key
;;   "ds" 'rw-desktop-save
;;   "dr" 'rw-desktop-read)

;; ;; clipboard: usually not used in windows...
;; (evil-leader/set-key
;;   "aa" 'copy-to-x-clipboard
;;   "zz" 'paste-from-x-clipboard)

(provide 'init-meow)
