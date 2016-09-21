;; export:
;; `rw-dos2unix' 

(defun rw-dos2unix (buffer)
  "Automate replace all ^M to nothing."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(provide 'rw-buffer-lib)
