;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall s-f)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window))))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key "\C-x1" 'sanityinc/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)


(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)

;; window numbering: move focus between sub-windows
(require 'init-window-numbering)

(defun rw/lift-frame-in-other-monitor ()
  "lift emacs-frame in other monitor"
  (interactive)
  (let ((cframe (selected-frame))
        (xframe (rw-select-frame-in-other-monitor)))
    (select-frame-set-input-focus xframe)
    (select-frame-set-input-focus cframe)))

(global-set-key (kbd "<f1>") 'rw/lift-frame-in-other-monitor)
(global-set-key (kbd "<f2>") 'pop-to-mark-command)

(defun rw/switch-to-next-frame-in-same-monitor (&optional frame)
  (interactive)
  (let* ((frame (or frame (selected-frame))))
    (select-frame-set-input-focus (rw-next-frame-in-same-monitor frame))))

(defun rw/switch-to-previous-frame-in-same-monitor (&optional frame)
  (interactive)
  (let* ((frame (or frame (selected-frame))))
    (select-frame-set-input-focus (rw-previous-frame-in-same-monitor frame))))

(global-set-key (kbd "C-M-<next>") 'rw/switch-to-next-frame-in-same-monitor)
(global-set-key (kbd "C-M-<prior>") 'rw/switch-to-previous-frame-in-same-monitor)

(provide 'init-windows)
