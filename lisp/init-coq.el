(defun coq-mode-func ()
  "features needed by coq mode"
  (rice-wine-prog-func)
  (yas-on)
  (company-coq-on)
  (cscope-minor-mode))

(use-package proof-site
  :load-path (lambda ()
               (expand-file-name "PG/generic"
                                 rice-wine-git-package-dir))
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
    (setq proof-general-debug nil))

  (use-package rw-frame-lib)

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

  ;; improve pg's *goals* and *respons* display
  (evil-leader/set-key
    "cl" 'rw/pg-show-goals-and-responds-in-other-frame)

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


  (add-hook 'coq-mode-hook 'coq-mode-func)
  (use-package rw-coq-lib
    :config
    (evil-leader/set-key
      "ap" 'lzh/coq-trans)))

(provide 'init-coq)
