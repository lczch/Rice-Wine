(require 'cl) ; Compatibility aliases for the old CL library Built-in
(require 'cl-lib) ; Common Lisp extensions for Emacs, Built-in

(defun rice-wine/add-subdirs-to-load-path (dir)
  "add all subdirs of DIR to load-path, using internal function
   normal-top-level-add-subdirs-to-load-path"
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defun rice-wine/add-to-load-path (dir)
  "add DIR to the head of load-path"
  (add-to-list 'load-path dir))

;; elisp version of try...catch...finally
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]*$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url-generic (concat "file://" (buffer-file-name))))


(require 'cl)

(defmacro with-selected-frame (frame &rest forms)
  (let ((prev-frame (gensym))
        (new-frame (gensym)))
    `(progn
       (let* ((,new-frame (or ,frame (selected-frame)))
              (,prev-frame (selected-frame)))
         (select-frame ,new-frame)
         (unwind-protect
             (progn ,@forms)
           (select-frame ,prev-frame))))))

(defvar load-user-customized-major-mode-hook t)
(defvar cached-normal-file-full-path nil)
(defun is-buffer-file-temp ()
  (interactive)
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
  (let ((f (buffer-file-name))
        org
        (rlt t))
    (cond
     ((not load-user-customized-major-mode-hook) t)
     ((not f)
      ;; file does not exist at all
      (setq rlt t))
     ((string= f cached-normal-file-full-path)
      (setq rlt nil))
     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))
     ((and (string-match "\.html$" f)
           (file-exists-p (setq org (replace-regexp-in-string "\.html$" ".org" f))))
      ;; file is a html file exported from org-mode
      (setq rlt t))
     (t
      (setq cached-normal-file-full-path f)
      (setq rlt nil)))
    rlt))

(defun my-guess-mplayer-path ()
  (let ((rlt "mplayer"))
    (cond
     (*is-a-mac* (setq rlt "mplayer -quiet"))
     (*linux* (setq rlt "mplayer -quiet -stop-xscreensaver"))
     (*cygwin*
      (if (file-executable-p "/cygdrive/c/mplayer/mplayer.exe")
          (setq rlt "/cygdrive/c/mplayer/mplayer.exe -quiet")
        (setq rlt "/cygdrive/d/mplayer/mplayer.exe -quiet")))
     (t ; windows
      (if (file-executable-p "c:\\\\mplayer\\\\mplayer.exe")
          (setq rlt "c:\\\\mplayer\\\\mplayer.exe -quiet")
        (setq rlt "d:\\\\mplayer\\\\mplayer.exe -quiet"))))
    rlt))

(defun my-guess-image-viewer-path (file &optional is-stream)
  (let ((rlt "mplayer"))
    (cond
     (*is-a-mac*
      (setq rlt
            (format "open %s &" file)))
     (*linux*
      (setq rlt
            (if is-stream (format "curl -L %s | feh -F - &" file) (format "feh -F %s &" file))))
     (*cygwin* (setq rlt "feh -F"))
     (t ; windows
      (setq rlt
            (format "rundll32.exe %SystemRoot%\\\\System32\\\\\shimgvw.dll, ImageView_Fullscreen %s &" file))))
    rlt))

;;------------------------------------------------------------------------------
;; my functions
;;------------------------------------------------------------------------------

;; manipulate frames
(defun rw-count-frames ()
  "count current frames, return a number"
  (let ((current-frame-list (frame-list)))
    (length current-frame-list)))

(defun rw-only-one-frame? ()
  "is there only one frame?"
  (let ((n (rw-count-frames)))
    (if (= n 1) t
      nil)))

(defun rw-get-monitor-name-of-frame (&optional frame)
  (let ((attributes
         (frame-monitor-attributes frame)))
    (cl-loop for attribute in attributes
             do (pcase attribute
                  (`(name . ,monitor-name)
                   (return monitor-name))
                  (_ nil))
             finally return nil)))

(defun rw-select-frame-in-other-monitor (&optional frame)
  "return the frame in other monitor comparing with frame, if no such frame, return nil"
  (let* ((frame (or frame (selected-frame)))
         (monitor1 (rw-get-monitor-name-of-frame frame)))
    (cl-loop for xframe = (next-frame frame) then (next-frame xframe)
             until (eql xframe frame)
             do
             (unless (string= (rw-get-monitor-name-of-frame xframe) monitor1)
               ;; xframe in different monitor with frame
               (return xframe))
             finally return nil)))

(defun rw-next-frame-in-same-monitor (&optional frame)
  "return the next frame in same monitor with frame, if no such frame, return frame itself"
  (let* ((frame (or frame (selected-frame)))
         (monitor1 (rw-get-monitor-name-of-frame frame)))
    (cl-loop for xframe = (next-frame frame) then (next-frame xframe)
             until (eql xframe frame)
             do
             (when (string= (rw-get-monitor-name-of-frame xframe) monitor1)
               ;; xframe in different monitor with frame
               (return xframe))
             finally return frame)))

(defun rw-previous-frame-in-same-monitor (&optional frame)
  "return the previous frame in same monitor with frame, if no such frame, return frame itself"
  (let* ((frame (or frame (selected-frame)))
         (monitor1 (rw-get-monitor-name-of-frame frame)))
    (cl-loop for xframe = (previous-frame frame) then (previous-frame xframe)
             until (eql xframe frame)
             do
             (when (string= (rw-get-monitor-name-of-frame xframe) monitor1)
               ;; xframe in different monitor with frame
               (return xframe))
             finally return frame)))

(provide 'init-utils)
