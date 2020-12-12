;; functions
(defun rw-add-to-load-path (dir)
  "add DIR to the head of load-path"
  (add-to-list 'load-path dir))

(defun rw-add-subdirs-to-load-path (dir)
  "add all subdirs of DIR to load-path, which begin with a digital or letter."
  (let ((dir-files (directory-files dir t "^[0-9A-Za-z].*")))
    (dolist (file dir-files)
      (when (file-directory-p file)
        (rw-add-to-load-path file)))))

(defun rw-add-dir-and-subdirs-to-load-path (dir)
  "add DIR and all subdirs of DIR to load-path, which begin with a digital or letter."
  (interactive "DDir:")
  (rw-add-to-load-path dir)
  (rw-add-subdirs-to-load-path dir))

(defun expand-directory-name (name &optional default-directory)
  "Same as `expand-file-name', but make sure the return value endes with slash."
  (let ((result (expand-file-name name default-directory)))
    (if (not (string-suffix-p "/" result))
        (concat result "/")
      result)))

;; global variables
(defvar rice-wine-dir (expand-directory-name (file-name-directory load-file-name))
  "top directory of configuration")

(defvar rice-wine-lisp-dir (expand-directory-name "layers" rice-wine-dir)
  "configurations of packages")

(defvar rice-wine-package-dir
  (expand-directory-name "site-lisp" rice-wine-dir)
  "local packages")

(defvar rice-wine-git-package-dir
  (expand-directory-name "git-lisp" rice-wine-dir)
  "packages from git, which have higher priority than pakages in `rice-wine-package-dir'")

(defvar rice-wine-lib-dir
  (expand-directory-name "lib" rice-wine-dir)
  "library packages, mostly for elisp programming")

;; load libs
(rw-add-dir-and-subdirs-to-load-path rice-wine-lib-dir)

(require 'dash)

(require 's)

(require 'f)
