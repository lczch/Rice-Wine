;; some useful elisp functions at top level

(defun rice-wine/add-subdirs-to-load-path (dir)
  "add all subdirs of DIR to load-path, using internal function
   normal-top-level-add-subdirs-to-load-path"
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defun rice-wine/add-to-load-path (dir)
  "add DIR to the head of load-path"
  (add-to-list 'load-path dir))

(provide 'init-rice-wine-functions)
