:; exec emacs -batch -l "$0" -f main -- "$@"

(load-file (expand-file-name "init-header.el" "~/rice-wine"))

(setq git-package-plists
      '((:name
         use-package
         :address
         "git@github.com:lczch/use-package.git")
        (:name
         PG
         :address
         "git@github.com:lczch/PG.git")
        (:name
         company-coq
         :address
         "git@github.com:lczch/company-coq.git")
        (:name
         racket-mode
         :address
         "git@github.com:lczch/racket-mode.git")
        (:name
         org-mode
         :address
         "git://orgmode.org/org-mode.git"
         :actions
         ("git checkout release_8.3.6"
          "make autoloads"))
        (:name
         slime
         :address
         "git@github.com:lczch/slime.git")
        (:name
         tuareg :address
         "https://github.com/ocaml/tuareg.git"
         :actions
         ("git checkout 2.0.8"))
        (:name
         smartparens
         :address
         "https://github.com/Fuco1/smartparens.git"
         :actions
         ("git checkout 1.9.0"))
        (:name
         haskell-mode
         :address
         "https://github.com/haskell/haskell-mode.git"
         :actions
         ("make"))
        (:name
         yasnippet
         :address
         "git@github.com:joaotavora/yasnippet.git"
         :actions
         ("git checkout 0.12.2"))
        ))


(setq git-dir rice-wine-git-package-dir)
;; (setq git-dir (expand-directory-name "~/rice-wine/test-git-lisp"))

(defun expand-directory-name (name &optional default-directory)
  "Same as `expand-file-name', but make sure the return value endes with slash."
  (let ((result (expand-file-name name default-directory)))
    (if (not (string-suffix-p "/" result))
        (concat result "/")
      result)))

(defun git-package-handler (plist)
  (let* ((name (symbol-name (plist-get plist :name)))
         (address (plist-get plist :address))
         (actions (plist-get plist :actions))
         (package-directory (expand-file-name name git-dir)))
    ;; start
    (let ((default-directory git-dir))
      (princ (format "Start clone: %s\n" name))
      (shell-command (concat "git clone " address))

      ;; run actions
      (let ((default-directory (expand-directory-name name git-dir)))
        (-each actions
          (lambda (it)
            (princ (format "Execute: %s\n" it))
            (shell-command it))))

      ;; finish
      (princ (format "Finish configure package: %s\n\n" name)))
    ))

(defun main ()
  (unless (file-exists-p git-dir)
    (mkdir git-dir))

  ;; (setq default-directory git-dir)
  (-each git-package-plists
    (lambda (it)
      (git-package-handler it))))


