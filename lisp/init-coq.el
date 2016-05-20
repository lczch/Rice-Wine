(defun rw/pg-show-goals-and-responds-in-other-frame ()
  "show buffer *goals* and *responds* in other frame.
   1. if there is frame in other monitor exists, then switch to that
      frame, rearrange it to show  *goals* and *responds* horizontally
   2. if there is only one frame, then create one, and
      perform same action as 1"
  (interactive)
  (delete-other-windows) ;; delete auto generate layout
  (let ((cframe (selected-frame))
        (xframe (or (rw-select-frame-in-other-monitor)
                    (make-frame))))
    (select-frame xframe)
    ;; now we in new frame
    (switch-to-buffer "*goals*")
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer "*response*")
    (other-window 1)
    (select-frame cframe)))

(evil-leader/set-key
  "cl" 'rw/pg-show-goals-and-responds-in-other-frame)


(add-to-list 'load-path
             (expand-file-name "PG/generic"
                               rice-wine-package-dir))
(use-package proof-site
  :mode ("\\.v\\'" . coq-mode)
  :config
  (setq
   proof-splash-enable nil
   coq-indent-semicolon-tactical 0
   coq-match-indent 4
   coq-one-command-per-line t
   proof-auto-raise-buffers nil ;; prevent closing the other frame when it only show *goals* and *responds*
   proof-multiple-frames-enable nil ;; this feature is buggy...
   proof-keep-response-history nil
   proof-next-command-insert-space t)

  (defun pg-debug-on ()
    (interactive)
    (setq proof-general-debug t))

  (defun pg-debug-off ()
    (interactive)
    (setq proof-general-debug nil)))



;; (let ((setup-file
;;        (expand-file-name "PG/generic/proof-site.el"
;;                          rice-wine-package-dir))
;;       (project-file
;;        (expand-file-name ".coq-project.el" rice-wine-dir)))
;;   (when (file-exists-p project-file)
;;     (load-file project-file))
;;   (load-file setup-file))

;;; setup company-coq
;; company-coq itself include setup of company and yasnippet,
;; this behaviour is a little annoying.
;; And its depends on a lot packages:
;;  company-math(on math-symbol-list), dash, noflet, altert(on gntp and log4e)...
;; This package should be made more primitive.

(use-package company-coq
  :commands (company-coq-mode company-coq-initialize)
  :init
  (defun company-coq-on ()
    (interactive)
    (company-coq-initialize))
  (defun company-coq-off ()
    (interacitve)
    (company-coq-mode 0))

  :config
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
  (setq company-coq-prettify-symbols-alist
        '(("|-" . 8866)
          ("->" . 8594)
          ("=>" . 8658)
          ("fun" . 955)
          ("forall" . 8704)
          ("exists" . 8707)
          ("/\\" . 8743)
          ("\\/" . 8744)
          ("~" . 172)
          ("+-" . 177)
          (">->" . 8611))))

(use-package coq
  :commands coq-mode
  :config
  (defun coq-mode-func ()
    (rice-wine-prog-func)
    (yas-on)
    (company-coq-on)
    (smartparens-on))
  (add-hook 'coq-mode-hook 'coq-mode-func)

  ;; useful functions
  (defun lzh/coq-grasp (lemma-name)
    "Grasp the whole text in *goals*, and transform it to a readable (for coq) Lemma"
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
    "auto comment all the assistant command"
    (interactive)
    (let ((init-p (point)))
      (goto-char (point-min))
      (while (re-search-forward lzh/coq-fuzz-regexp nil t nil)
        (replace-match "\\1(* ** ac: \\2 *)"))
      (goto-char init-p))))


(provide 'init-coq)
