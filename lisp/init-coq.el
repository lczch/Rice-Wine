;;; setup proof-general
(let ((setup-file
       (expand-file-name "PG/generic/proof-site.el"
				    rice-wine-package-dir)))
  (load-file setup-file))

;;; setup company-coq
;; company-coq itself include setup of company and yasnippet,
;; this behaviour is a little annoying.
;; And its depends on a lot packages:
;;  company-math(on math-symbol-list), dash, noflet, altert(on gntp and log4e)...
;; This package should be made more primitive.
(require 'company-coq)

(defun turn-on-company-coq ()
  (company-coq-initialize))

(setq company-coq-disabled-features
      '(snippets
        outline
        code-folding
        company-defaults
        ;;refman-ltac-abbrevs-backend
        ;;refman-tactic-abbrevs-backend
        ;;refman-vernac-abbrevs-backend
        refman-scope-abbrevs-backend
        pg-backend
        dynamic-symbols-backend
        obsolete-settings))

(add-hook 'coq-mode-hook 'run-rice-wine-prog-hook)
(add-hook 'coq-mode-hook 'turn-on-company-coq)


;; useful functions
(defun lzh/coq-grasp (lemma-name)
  "Grasp the whole text in *goals*, and transform it to a readable (for coq) Lemma"
  (interactive "sname:")
  (let ((temp-buffer (get-buffer-create "*temp-buffer*")))
    (with-current-buffer temp-buffer
      (insert-buffer-substring "*goals*")
      (goto-char (point-min))
      ;; delete first 2 lines
      (kill-whole-line)
      (kill-whole-line)
      ;; insert the lemma header
      (re-search-forward "^.*$")
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

(defconst lzh/coq-search-regexp "\\(?:Search.*[.]$\\)")
(defconst lzh/coq-print-regexp "\\(?:Print.*[.]$\\)")
(defconst lzh/coq-check-regexp "\\(?:Check.*[.]$\\)")
(defconst lzh/coq-locate-regexp "\\(?:Locate.*[.]$\\)")
(defconst lzh/coq-show-regexp "\\(?:Show.*[.]$\\)")
(defconst lzh/coq-idtac-regexp "\\(?:idtac .*[.;]$\\)")

(defconst lzh/coq-fuzz-regexp
  (concat "\\(^[[:blank:]]*\\)\\("
          lzh/coq-search-regexp "\\|"
          lzh/coq-print-regexp "\\|"
          lzh/coq-check-regexp "\\|"
          lzh/coq-locate-regexp "\\|"
          lzh/coq-show-regexp "\\|"
          lzh/coq-idtac-regexp "\\)"))

(defun lzh/coq-beautify ()
  "auto comment all the assistant command"
  (interactive)
  (let ((init-p (point)))
    (goto-char (point-min))
    (while (re-search-forward lzh/coq-fuzz-regexp nil t nil)
      (replace-match "\\1(* ** ac: \\2 *)"))
    (goto-char init-p)))


(provide 'init-coq)
