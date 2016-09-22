;; export:
;; `lzh/coq-grasp'
;; `lzh/coq-beautify'
;; `lzh/coq-trans': only for a specific use.
;; `lzh/coq-hint-resolve-backward-lemma'
;; `lzh/coq-create-dir-locals'

(defun lzh/coq-grasp (lemma-name)
  "Grasp the whole text in *goals*, and transform it to a readable (for coq) Lemma, using LEMMA-NAME as the name of new lemma.
   And push it on the top of kill-ring"
  (interactive "sname:")
  (let ((temp-buffer (get-buffer-create "*temp-buffer*")))
    (with-current-buffer temp-buffer
      (insert-buffer-substring "*goals*")
      (goto-char (point-min))
      ;; delete the thing above a blank line
      (let ((blank-line (re-search-forward "^[[:blank:]]*$")))
        (delete-region (point-min) blank-line))
      ;; insert the lemma header
      (re-search-forward "^[[:blank:]]*$")
      (replace-match (concat "Lemma " lemma-name ": forall"))
      ;; deal with spliter
      (let ((split-point nil))
        (let ((init-p (point)))
          (re-search-forward "^[ =]*$")
          (replace-match "(\* ================================= \*) ,")
          (forward-line -1)
          (end-of-line)
          (insert ")")
          (setq split-point (point-marker))
          (goto-char init-p))
        ;; add parenthesis
        (re-search-forward " : "  nil t)
        (beginning-of-line-text)
        (insert "(")
        (end-of-line)
        (while (and
                (re-search-forward " : " nil t)
                (< (point) (marker-position split-point)))
          ;; first
          (beginning-of-line-text)
          (insert "(")
          ;; last
          (forward-line -1)
          (end-of-line)
          (insert ")")
          ;; move point
          (forward-line 2)))
      ;; add the last "."
      (let ((init-p (point)))
        (goto-char (point-max))
        (insert ".")
        (goto-char init-p))
      ;; add whole text to kill-ring
      (kill-new (buffer-string))
      (kill-buffer temp-buffer))))

(defconst lzh/coq-search-regexp "\\(?:Search.*[.]\\)")
(defconst lzh/coq-print-regexp "\\(?:Print.*[.]\\)")
(defconst lzh/coq-check-regexp "\\(?:Check.*[.]\\)")
(defconst lzh/coq-locate-regexp "\\(?:Locate.*[.]\\)")
(defconst lzh/coq-show-regexp "\\(?:Show.*[.]\\)")
(defconst lzh/coq-idtac-regexp "\\(?:idtac .*[.;]\\)")

(defconst lzh/coq-fuzz-regexp
  (concat "\\(^[[:blank:]]*\\)\\("
          lzh/coq-search-regexp "\\|"
          lzh/coq-print-regexp "\\|"
          lzh/coq-check-regexp "\\|"
          lzh/coq-locate-regexp "\\|"
          lzh/coq-show-regexp "\\)"))

(defun lzh/coq-beautify ()
  "auto comment all the assistant command, base on `lzh/coq-fuzz-regexp'"
  (interactive)
  (let ((init-p (point)))
    (goto-char (point-min))
    (while (re-search-forward lzh/coq-fuzz-regexp nil t nil)
      (replace-match "\\1(* ** ac: \\2 *)"))
    (goto-char init-p)))

(defconst lzh/coq-lemma-name-regexp
  "Lemma[ ]+\\([a-zA-Z0-9_']*\\)")

(defun lzh/coq-get-lemma-name-backward ()
  "Get lemma name just above to the cursor."
  (interactive)
  (save-excursion
    (let ((search (re-search-backward lzh/coq-lemma-name-regexp)))
      (match-string 1))))

(defun lzh/coq-lemma-new-name (name)
  (concat name "_auto"))

(defun lzh/coq-get-lemma-name-forward ()
  "Get lemma name just below to the cursor."
  (interactive)
  (save-excursion
    (let ((search (re-search-forward lzh/coq-lemma-name-regexp)))
      (match-string 1))))

(defun lzh/coq-get-lemma-forward ()
  "Get the whole lemma definition just below to the cursor."
  (interactive)
  (save-excursion
    (let* ((search (re-search-forward "Lemma"))
           (start (match-beginning 0))
           (search1 (re-search-forward "[.]$"))
           (end (match-end 0)))
      (buffer-substring-no-properties start end))))

(defun lzh/coq-trans-admit ()
  (interactive)
  (save-excursion
    (let* ((name (lzh/coq-get-lemma-name-forward))
           (name1 (lzh/coq-lemma-new-name name))
           (search (re-search-forward "Admitted[.]"))
           (new (concat "  intros; eapply " name1 "; ica.\n" "Qed.")))
      (replace-match new t t))))

(defun lzh/coq-trans-lemma ()
  (interactive)
  (save-excursion
    (let* ((text (lzh/coq-get-lemma-forward))
           (text-lines (vconcat (split-string text "$"))))
      (let* ((line (aref text-lines 0))
             (search (string-match lzh/coq-lemma-name-regexp line))
             (name (match-string 1 line))
             (new-name (lzh/coq-lemma-new-name name))
             (new-line (replace-match new-name nil nil line 1)))
        (aset text-lines 0 new-line))
      (let* ((line (aref text-lines 1))
             (search1 (string-match "(\\(.*\\)[ ]*:[ ]*\\(.*\\))" line))
             (type (match-string 2 line))
             (new-line1 (replace-match "\\1" nil nil line 0))
             (search2 (string-match "forall" new-line1))
             (new-line2 (replace-match "forall (A B T : Type) (MC : PermMap A B T)" nil t new-line1 0)))
        ;; (insert "\n" type)
        (if (string-equal type "mem")
            (setq new-line2 (concat new-line2 "\n    usePerm = true ->"))
          (setq new-line2 (concat new-line2 "\n    usePerm = false ->")))
        (aset text-lines 1 new-line2))
      (insert "\n")
      (loop for line across text-lines 
            do (insert line))
      (insert "\n  hy.\n" "Qed.\n"))))

(defun lzh/coq-trans ()
  "Transform old style join-lemma to new form join-lemma."
  (interactive)
  (lzh/coq-trans-admit)
  (lzh/coq-trans-lemma)
  (re-search-forward "Qed[.]" nil t 2)
  (forward-line))

(defun lzh/coq-hint-resolve-backward-lemma (&optional hint-database)
  "Insert string |Hint Resolve lemma-name: HINT-DATABASE.| basing on the lemma above to cursor.
 HINT-DATABASE must be a string, naming a hint database name."
  (interactive)
  (let ((lemma-name (lzh/coq-get-lemma-name-backward)))
    (insert "Hint Resolve " lemma-name ": ")
    (when hint-database
      (insert hint-database "."))))


(defmacro lzh/coq-create-sexp-of-dir-locals (coq-prog-name coq-prog-args)
  `((nil
     . ((eval
         . (progn
             (setq coq-prog-name ,coq-prog-name)
             (setq coq-prog-args ',coq-prog-args)))))))

(defun lzh/coq-create-dir-locals (dir coq-prog-name coq-prog-args)
  "Create a dir-locals file in DIR, base on COQ-PROG-NAME COQ-PROG-ARGS"
  (with-temp-file (expand-file-name dir-locals-file dir)
    (print
     (macroexpand `(lzh/coq-create-sexp-of-dir-locals ,coq-prog-name ,coq-prog-args))
     (current-buffer))))


(provide 'rw-coq-lib)
