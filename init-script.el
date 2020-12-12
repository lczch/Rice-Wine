;; 专为写脚本而配置

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

;; add needed dirs to load-path
(defvar rice-wine-dir (file-name-directory load-file-name)
  "top directory of configuration")

(defvar rice-wine-lisp-dir (expand-file-name "layers" rice-wine-dir)
  "configurations of packages")

(defvar rice-wine-package-dir
  (expand-file-name "site-lisp" rice-wine-dir)
  "local packages")

(defvar rice-wine-git-package-dir
  (expand-file-name "git-lisp" rice-wine-dir)
  "packages from git")

(defvar rice-wine-lib-dir
  (expand-file-name "lib" rice-wine-dir)
  "library packages, mostly for elisp programming")

(rw-add-to-load-path rice-wine-dir)
(rw-add-dir-and-subdirs-to-load-path rice-wine-lib-dir)

;; 经常要用到的库
(require 'cl)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)

(defun rw-get-args ()
  "拿到命令行中的参数"
  (cdr command-line-args-left))

(defun rw-get-script-file ()
  "返回正在执行的脚本的文件名, 绝对路径"
  (expand-file-name (nth 2 command-line-args)))


