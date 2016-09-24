;;------------------------------------------------------------------------------
;; I think the project-file in PG is not enough: it can't specify multi-version
;; of coq.
;; So I plan to write for myself.
;;------------------------------------------------------------------------------
(defvar rw-coq-project-filename ".rw-coq-project.el"
  "Name of project file.")

;; borrow from proof-general
(defun rw-coq-find-project-file ()
  "Return '(buf alreadyopen) where buf is the buffer visiting coq project file.
alreadyopen is t if buffer already existed."
  (let ((current-buffer-file-name (buffer-file-name)))
    (when current-buffer-file-name
      (let ((projectfiledir (locate-dominating-file current-buffer-file-name rw-coq-project-filename)))
        (when projectfiledir
          (let* ((projectfile (expand-file-name rw-coq-project-filename projectfiledir))
                 ;; we store this intermediate result to know if we have to kill
                 ;; the coq project buffer at the end
                 (projectbufferalreadyopen (find-buffer-visiting projectfile))
                 (projectbuffer (or projectbufferalreadyopen
                                    (find-file-noselect projectfile t t))))
            (list projectbuffer
                  (if projectbufferalreadyopen t
                    nil))))))))

(defvar rw-coq-project-file-debug t)

(defun rw-coq-project-debug (format-string &rest args)
  "Output debug messages."
  (when rw-coq-project-file-debug
    (apply #'message format-string args)))

(defun rw-coq-eval-project-file ()
  "Find rw-project-file and eval it contents."
  (save-excursion
    (let* ((temp (rw-coq-find-project-file))
           (project-file-buffer (first temp))
           (already-open (second temp)))
      (if project-file-buffer
          (progn
            (rw-coq-project-debug "rw-coq-project-file: find project file.")
            (with-current-buffer project-file-buffer
              (rw-coq-project-debug "rw-coq-project-file: \n%s" (buffer-file-name))
              ;; eval it contents
              (eval-buffer)
              ;; if this buffer is newly created, then kill it
              (unless already-open
                (kill-buffer))))
        (message "RW: coq project file %s is not found!!!!" rw-coq-project-filename)
        nil))))

(defun rw-advice-before-activating-scripting (&optional nosaves queuemode)
  "Before activating scripting, it is necessary to eval project file getting correct coq execution."
  (unless (equal (current-buffer) proof-script-buffer)
    ;; no activate script associated with current buffer
    (rw-coq-project-debug "Advice begin!!!!!")
    (rw-coq-eval-project-file)
    (rw-coq-project-debug "Advice end!!!!!")))

;; Enforce load the definition of `proof-activate-scripting'
(require 'proof-script)
;; add this advice before `proof-activate-scripting'
(rw-coq-project-debug "Is symbol-function of proof-activate-scripting not nil? %s"
                      (if (symbol-function 'proof-activate-scripting) t nil))

(add-function :before (symbol-function 'proof-activate-scripting)
              #'rw-advice-before-activating-scripting)

;;------------------------------------------------------------------------------
;; create a project file and open it
;;------------------------------------------------------------------------------
(defun rw-coq-create-project-file (dir)
  (interactive "DDir:")
  (let ((project-file (expand-file-name rw-coq-project-filename dir)))
    (find-file project-file)
    (print
     '(setq coq-prog-name (expand-file-name "coqtop" (getenv "coq85bin")))
     (current-buffer))
    (print
     '(setq coq-prog-args `("-R" ,(file-name-directory (buffer-file-name)) "CertiOS"))
     (current-buffer))))

(provide 'rw-pg-project-file)
