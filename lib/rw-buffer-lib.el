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

(provide 'rw-buffer-lib)
