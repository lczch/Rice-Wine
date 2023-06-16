;;------------------------------------------------------------------------------
;; misc functions
;;------------------------------------------------------------------------------


;; 失败, 完全没有达到禁止org-roam加载的效果.
;; 这是因为--eval是在加载完配置之后eval的, 所以就不行. 
(defun rw-test-new-config ()
  "Async open a new emacs, with current file opened and initial `not-test' as nil."
  (interactive)
  ;; (when (f-exists? rice-wine-configure-file)
  ;;   (org-babel-tangle-file rice-wine-configure-file))
  ;; start emacs
  (let ((file (buffer-file-name)))
    (async-shell-command (concat "emacs " file " --eval \"(setq not-test nil)\"" " --debug"))))

(defun rw/dnd ()
  "100面的骰子"
  (interactive)
  (insert (format "骰子结果: %d" (+ (random 100) 1))))


;; convert between english and chinese punctuation
(defun rw/xah-convert-english-chinese-punctuation (*begin *end &optional *to-direction)
  "Convert punctuation from/to English/Chinese characters.

When called interactively, do current line or selection. The conversion direction is automatically determined.

If `universal-argument' is called, ask user for change direction.

When called in lisp code, *begin *end are region begin/end positions. *to-direction must be any of the following values: 「\"chinese\"」, 「\"english\"」, 「\"auto\"」.

See also: `xah-remove-punctuation-trailing-redundant-space'.

URL `http://ergoemacs.org/emacs/elisp_convert_chinese_punctuation.html'
Version 2015-10-05"
  (interactive
   (let (-p1 -p2)
     (if (use-region-p)
         (progn
           (setq -p1 (region-beginning))
           (setq -p2 (region-end)))
       (progn
         (setq -p1 (line-beginning-position))
         (setq -p2 (line-end-position))))
     (list
      -p1
      -p2
      (if current-prefix-arg
          (ido-completing-read
           "Change to: "
           '( "english"  "chinese")
           "PREDICATE"
           "REQUIRE-MATCH")
        "auto"
        ))))
  (let (
        (-input-str (buffer-substring-no-properties *begin *end))
        (-replacePairs
         [
          [". " "。"]
          [".\n" "。\n"]
          [", " "，"]
          [",\n" "，\n"]
          [": " "："]
          ["; " "；"]
          ["? " "？"] ; no space after
          ["! " "！"]

          ;; for inside HTML
          [".</" "。</"]
          ["?</" "？</"]
          [":</" "：</"]
          ;; [" " "　"]
          ]
         ))

    (when (string= *to-direction "auto")
      (setq
       *to-direction
       (if
           (or
            (string-match "　" -input-str)
            (string-match "。" -input-str)
            (string-match "，" -input-str)
            (string-match "？" -input-str)
            (string-match "！" -input-str))
           "english"
         "chinese")))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (mapc
         (lambda (-x)
           (progn
             (goto-char (point-min))
             (while (search-forward (aref -x 0) nil "noerror")
               (replace-match (aref -x 1)))))
         (cond
          ((string= *to-direction "chinese") -replacePairs)
          ((string= *to-direction "english") (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) -replacePairs))
          (t (user-error "Your 3rd argument 「%s」 isn't valid" *to-direction))))))))

(defun rw/xah-remove-punctuation-trailing-redundant-space (*begin *end)
  "Remove redundant whitespace after punctuation.
Works on current line or text selection.

When called in emacs lisp code, the *begin *end are cursor positions for region.

See also `xah-convert-english-chinese-punctuation'.

URL `http://ergoemacs.org/emacs/elisp_convert_chinese_punctuation.html'
version 2015-08-22"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (require 'xah-replace-pairs)
  (xah-replace-regexp-pairs-region
   *begin *end
   [
    ;; clean up. Remove extra space.
    [" +," ","]
    [",  +" ", "]
    ["?  +" "? "]
    ["!  +" "! "]
    ["\\.  +" ". "]

    ;; fullwidth punctuations
    ["， +" "，"]
    ["。 +" "。"]
    ["： +" "："]
    ["？ +" "？"]
    ["； +" "；"]
    ["！ +" "！"]
    ["、 +" "、"]
    ]
   "FIXEDCASE" "LITERAL"))


(provide 'rw-misc-lib)
