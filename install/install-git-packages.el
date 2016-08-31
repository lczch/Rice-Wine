:; exec emacs -batch -l "$0" -f main -- "$@"

(setq my-packages '((use-package . "git@github.com:lczch/use-package.git")
                    (PG . "git@github.com:lczch/PG.git")
                    (company-coq . "git@github.com:lczch/company-coq.git")
                    (racket-mode . "git@github.com:lczch/racket-mode.git")
                    (org-mode . "git://orgmode.org/org-mode.git")
                    (slime . "git@github.com:lczch/slime.git")))

(setq git-dir (concat (expand-file-name "~/rice-wine/git-lisp") "/"))
;;(setq git-dir (concat (expand-file-name "~/rice-wine/get") "/"))

(unless (file-exists-p git-dir)
  (mkdir git-dir))

(setq default-directory git-dir)

(defun main ()
  (dolist (elt my-packages)
    (let* ((name (symbol-name (car elt)))
           (url (cdr elt))
           (file-name (concat git-dir name)))
      (if (not (file-exists-p file-name))
          (progn
            ;; file not exits, need clone
            (princ (format "start clone: %s\n" name))
            (shell-command (concat "git clone " url))
            (if (string= name "org-mode")
                (let ((default-directory (concat git-dir "org-mode/")))
                  ;; keep to the latest stable release of org-mode
                  (shell-command "git checkout -b stable origin/maint")
                  (shell-command "make autoloads")
                  (princ (format "checkout to stable branch, and make autoloads\n"))))
            (princ (format "finish clone: %s\n" name))
            )
        ;; file exists
        (princ (format "%s is already exits!\n" name))))))


