;; export:
;; `rw-dos2unix' 
;; `rw-display-current-buffer-other-frame'

(defun rw-dos2unix (buffer)
  "Automate replace all ^M to nothing."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(defun rw-display-current-buffer-other-frame ()
  "display current buffer on other frame"
  (interactive)
  (display-buffer-other-frame (current-buffer)))

(defun rw-window-relayout (buffer1 buffer2)
  "按 buffer1 | buffer2的方式重新组织屏幕."
  (switch-to-buffer buffer1)
  (delete-other-windows)
  (split-window-horizontally)
  (switch-to-buffer-other-window buffer2))

(provide 'rw-buffer-lib)
