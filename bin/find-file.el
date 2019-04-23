:; exec emacs -batch -l "$0" -f main -- "$@"
(load-file "~/rice-wine/init-script.el")

;; As with most programs, the special argument ‘--’ says that all
;; subsequent arguments are file names, not options, even if they start
;; with ‘-’.

;; -- 是为了防止 "-display" 这样以"-"开头的参数也被emacs解析

;; 可以通过(nth 2 command-line-args)拿到正在此执行文件的绝对路径

;; 从-batch进入emacs, noninteractive的值为t

(require 'rw-file-lib)

(defun main ()
  (print (rw-get-script-file))
  (--each
      (apply 'rw-f-entries-reg (rw-get-args))
    (princ (format "%s\n" it)))
  (princ "\n"))



