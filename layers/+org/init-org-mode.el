;; (require 'org)

;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

;; {{ NO spell check for embedded snippets

(use-package org
  :init
  (rw-add-to-load-path (expand-file-name "org-mode/lisp" rice-wine-git-package-dir))
  (rw-add-to-load-path (expand-file-name "org-mode/contrib/lisp" rice-wine-git-package-dir))
  
  ;; :commands (org-mode)
  :config
  (require 'org-agenda)

  ;; 让latex公式可以自动indent
  (add-hook 'org-mode-hook 'org-indent-mode)
  (use-package org-edit-latex
    ;; 使用latex的临时buffer来edit latex公式.
    :ensure t
    :config)

  ;; preserve breaks in the org file
  (setq org-export-preserve-breaks t)
  
  (use-package cdlatex
    ;; 我只需要cdlatex在org-mode中插入snippet的功能, 其他都不想要.
    ;; 本来还想在latex里试试cdlatex, 但它竟然把我的缩进键绑定没了! 再见!
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-cdlatex-mode)
    (define-key cdlatex-mode-map "(" nil))
  
  (use-package org-fragtog
    ;; 自动preview latex
    ;; 从git安装, 如果从melpa安装, 可能会安装org为依赖, 我不需要.
    :config
    (add-hook 'org-mode-hook 'org-fragtog-mode))
  
  ;; 尝试一下org的cdlatex
  ;; (add-hook 'org-mode 'turn-on-org-cdlatex)
  (use-package good-scroll
    ;; 解决有内嵌图片时的滚动问题
    :ensure t
    :config
    (add-hook 'org-mode-hook 'good-scroll-mode))
  
  (setq org-return-follows-link t)

  ;; 从剪贴板中粘贴图片, 现在只在windows中有效, 需要imagemagick.
  ;; https://emacs-china.org/t/markdown/9296/3
  ;; 在windows上, 使用命令`magick clipboard: test.png'可以将clipboard中的图片读取保存为test.png.
  (setq org-image-actual-width '(800))
  (setq org-startup-with-inline-images t)
  (defun org-insert-picture-clipboard ()
    (interactive)
    (let* ((image-dir
	    (if (not (buffer-file-name))
	        (cond ((string-prefix-p "CAPTURE-[0-9]" (buffer-name))
		       (let ((buffer-name (replace-regexp-in-string "CAPTURE-[0-9-]*" "" (buffer-name))))
		         (concat (file-name-directory (buffer-file-name (get-file-buffer buffer-name))) "images")))
		      (t (yank) (error "")))
	      "images"))
	   (fname (concat (make-temp-name "image-") (format-time-string "%Y%m%d-%H%M%S")))
	   (image-file (concat image-dir "/" fname ".png")))
      
      (unless (file-exists-p image-dir) (make-directory image-dir))
      ;; 将剪贴板中的图片保存为image-file
      (call-process "magick" nil nil nil
		    "clipboard:" image-file)
      (insert (format "[[file:%s]]" image-file))
      (org-display-inline-images)
      ))
  
  (use-package org-download
    :ensure t
    :disabled 
    :config
    (add-hook 'dired-mode-hook 'org-download-enable)
    (setq-default org-download-method 'directory
                  org-download-heading-lvl nil
                  org-download-image-dir "./images"
                  org-download-screenshot-method "imagemagick/convert"
                  org-download-timestamp ""
                  org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))
  
  (use-package htmlize
    :ensure t)

  (defcustom centaur-prettify-org-symbols-alist
    '(("[ ]" . ?☐)
      ("[X]" . ?☑)
      ("[-]" . ?⛝)

      ("#+ARCHIVE:" . ?📦)
      ("#+AUTHOR:" . ?👤)
      ("#+CREATOR:" . ?💁)
      ("#+DATE:" . ?📆)
      ("#+DESCRIPTION:" . ?⸙)
      ("#+EMAIL:" . ?📧)
      ("#+OPTIONS:" . ?⛭)
      ("#+SETUPFILE:" . ?⛮)
      ("#+TAGS:" . ?🏷)
      ("#+TITLE:" . ?📓)
      ("#+title:" . ?📓)

      ;; ("#+BEGIN_SRC" . ?✎)
      ;; ("#+END_SRC" . ?□)
      ("#+BEGIN_SRC" . ?⏠)
      ("#+END_SRC" . ?⏡)
      
      ;; ("#+BEGIN_QUOTE" . ?»)
      ;; ("#+END_QUOTE" . ?«)
      ("#+BEGIN_QUOTE" . ?❝)
      ("#+END_QUOTE" . ?❞)
      
      ("#+HEADERS" . ?☰)
      ("#+RESULTS:" . ?💻))
    "Alist of symbol prettifications for `org-mode'."
    :group 'centaur
    :type '(alist :key-type string :value-type (choice character sexp)))

  (add-hook 'org-mode-hook 'org-redisplay-inline-images)

  (add-hook 'org-mode-hook
            (lambda ()
              "Beautify org symbols."
              (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)
              (prettify-symbols-mode 1)))

  (add-hook 'org-indent-mode-hook
            (lambda()
              (diminish 'org-indent-mode)
              ;; WORKAROUND: Prevent text moving around while using brackets
              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
              (make-variable-buffer-local 'show-paren-mode)
              (setq show-paren-mode nil)))

  (use-package ox-gfm
    :ensure t)
  (add-to-list 'org-export-backends 'md)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  (use-package org-superstar
    :ensure t
    :if (char-displayable-p ?⚫)
    :commands (org-superstar-mode)
    :init
    (setq org-superstar-headline-bullets-list '("⚫" "⚫" "⚫" "⚫"))
    (add-hook 'org-mode-hook 'org-superstar-mode))

  (use-package org-fancy-priorities
    :disabled 
    :ensure t
    :commands (org-fancy-priorities-mode)
    :init
    (setq org-fancy-priorities-list
          (if (char-displayable-p ?⯀)
              '("⯀" "⯀" "⯀" "⯀")
            '("HIGH" "MEDIUM" "LOW" "OPTIONAL")))
    (add-hook 'org-mode-hook 'org-fancy-priorities-mode)
    )

  (use-package org-rich-yank
    :ensure t
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; org-brain
  (use-package org-brain
    :disabled 
    :ensure t
    :init
    (setq org-brain-path "~/note/brain")
    :config
    (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
    (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
    
    ;; (defvar org-capture-templates)
    
    ;; (push '("b" "Brain" plain (function org-brain-goto-end)
    ;;         "* %i%?" :empty-lines 1)
    ;;       org-capture-templates)
    
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 12)
    (setq org-brain-include-file-entries nil
          org-brain-file-entries-use-title nil)
    (setq org-brain-scan-for-header-entries nil)
    
    ;; Allows you to edit entries directly from org-brain-visualize    
    (use-package polymode
      :ensure t 
      :config
      (add-hook 'org-brain-visualize-mode-hook 'org-brain-polymode))
    )


  (use-package org-roam
    :ensure t
    :bind (:map org-roam-mode-map
                (("C-c n l" . org-roam)
                 ("C-c n f" . org-roam-find-file)
                 ("C-c n g" . org-roam-graph))
                :map org-mode-map
                (("C-c n i" . org-roam-insert))
                (("C-c n I" . org-roam-insert-immediate)))
    :init (org-roam)
    :config
    ;; 它需要的seq库太新了, 而seq库是emacs自带的, 所以....也许可以通过melpa下一个更新的, 但我更粗暴一些.
    (use-package my-seq)
    (use-package org-roam-protocol)
    (use-package emacsql
      :ensure t)
    (use-package emacsql-sqlite3
      :ensure t
      :custom (emacsql-sqlite-executable-path "C:\\\\msys64\\\\home\\\\lzh\\\\bin\\\\sqlite3.exe"))

    (defun my-org-roam-capture-new-task (arg)
      "直接创造一个task, 归档在默认的在agenda文件中. 我这里是硬编码的."
      (interactive "s work(a)|emergency(b)|love(d)|reading(r)|technique(t)|game(g)|fiction(f)|cook(o)|fun(c)|travel(e)|festival(z)?")
      (let* ((title "task")
             (org-roam-capture--info (list (cons 'title title)
                                            (cons 'slug (funcall org-roam-title-to-slug-function title))
                                            (cons 'file "~/zettelkasten/task.org")))
             (org-roam-capture--context 'capture))
        (cond
         ((s-equals? arg "a")
          (org-roam-capture--capture nil "ta"))
         ((s-equals? arg "b")
          (org-roam-capture--capture nil "tb"))
         ((s-equals? arg "d")
          (org-roam-capture--capture nil "td"))
         ((s-equals? arg "r")
          (org-roam-capture--capture nil "tr"))
         ((s-equals? arg "t")
          (org-roam-capture--capture nil "tt"))
         ((s-equals? arg "g")
          (org-roam-capture--capture nil "tg"))
         ((s-equals? arg "f")
          (org-roam-capture--capture nil "tf"))
         ((s-equals? arg "o")
          (org-roam-capture--capture nil "to"))
         ((s-equals? arg "c")
          (org-roam-capture--capture nil "tc"))
         ((s-equals? arg "e")
          (org-roam-capture--capture nil "te"))
         ((s-equals? arg "z")
          (org-roam-capture--capture nil "tz"))
         (t (message arg)))
        ))
    
    (setq org-roam-directory "~/zettelkasten")
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))

    (setq org-roam-capture-templates
          '(("d" "default" plain #'org-roam-capture--get-point "%?"
             :file-name "%<%Y%m%d%H%M%S>"
             :head "#+title: ${title}\n#+roam_tags: \n#+roam_alias: \n\n" :unnarrowed t)
            ("t" "group")
            ("ta" "work task" plain #'org-roam-capture--get-point
             "* TODO %?    :work:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("tb" "emergent task" plain #'org-roam-capture--get-point
             "* TODO %?    :emergency:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("tc" "fun task" plain #'org-roam-capture--get-point
             "* TODO %?    :fun:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("td" "love" plain #'org-roam-capture--get-point
             "* TODO %?    :love:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("tg" "game" plain #'org-roam-capture--get-point
             "* TODO %?    :game:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("tr" "reading" plain #'org-roam-capture--get-point
             "* TODO %?    :reading:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("tf" "fiction" plain #'org-roam-capture--get-point
             "* TODO %?    :fiction:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("tt" "technique" plain #'org-roam-capture--get-point
             "* TODO %?    :technique:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("to" "cook" plain #'org-roam-capture--get-point
             "* TODO %?    :cook:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("te" "travel" plain #'org-roam-capture--get-point
             "* TODO %?    :travel:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ("tz" "festival" plain #'org-roam-capture--get-point
             "* TODO %?    :festival:"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_tags: \n\n"
             :unarrowed t
             :empty-lines 1)
            ))

    (setq org-roam-capture-immediate-template
          '("d" "default" plain #'org-roam-capture--get-point "%?"
            :file-name "%<%Y%m%d%H%M%S>"
            :head "#+title: ${title}\n#+roam_tags:\n\n"
            :unnarrowed t
            :immediate-finish t))

    ;; https://emacs-china.org/t/org-roam-error-running-timer-org-roam-db-update-cache-on-timer-error-selecting-deleted-buffer/15346/2
    (setq org-roam-db-update-method 'immediate)
    (org-roam-mode)
    )

  (use-package org-super-agenda
    :ensure t
    :init
    (setq org-agenda-files `(,(expand-file-name "~/zettelkasten/task.org")))
    (setq org-indirect-buffer-display 'dedicated-frame)
    (setq org-agenda-include-diary nil)
    
    (bind-keys
     :map org-agenda-mode-map
     ("RET" . org-agenda-tree-to-indirect-buffer))
    
    (define-key global-map (kbd "C-c a") 'org-agenda)
    :config
    ;; (defun my-org-super-agenda-transformer (str)
    ;;   (let* ((str1 (s-trim str))
    ;;          (str2 (s-collapse-whitespace str1))
    ;;          (strs (s-split " " str2))
    ;;          (str4 (s-join " " (seq-subseq strs 1 -1))))
    ;;     (concat "  " str4)))

    ;; 用了transformer会导致从agenda定位不到task所在的文件...
    (setq org-super-agenda-groups
          '(;; Each group has an implicit boolean OR operator between its selectors.
            (:name "Work"
                   :tag "work"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Emergency"
                   :tag "emergency"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Love"
                   :tag "love"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Reading"
                   :tag "reading"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Game"
                   :tag "game"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Fiction"
                   :tag "fiction"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Cook"
                   :tag "cook"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Technique"
                   :tag "technique"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Fun"
                   :tag "fun"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Travel"
                   :tag "travel"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            (:name "Festival"
                   :tag "festival"
                   ;; :transformer my-org-super-agenda-transformer
                   )
            ))
    (add-hook 'org-mode-hook 'org-super-agenda-mode)

    (defun rw-window-relayout (buffer1 buffer2)
      "按 buffer1 | buffer2的方式重新组织屏幕."
      (switch-to-buffer buffer1)
      (delete-other-windows)
      (split-window-horizontally)
      (switch-to-buffer-other-window buffer2))
    )

  
  (use-package org-roam-server
    :ensure t
    :disabled
    :config
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 9090
          org-roam-server-authenticate nil
          org-roam-server-export-inline-images t
          ;; org-roam-server-serve-files nil
          ;; org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
          org-roam-server-network-poll t
          ;; org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20)
    
    (org-roam-server-mode)
    )

  (defun org-mode-is-code-snippet ()
    (let (rlt
          (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
          (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
          (old-flag case-fold-search)
          b e)
      (save-excursion
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt t))
      rlt))

  ;; no spell check for property
  (defun org-mode-current-line-is-property ()
    (let (cur-line)
      (setq cur-line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
      ;; (message "cur-line=%s" cur-line)
      (string-match "^[ \t]+:[A-Z]+:[ \t]+" cur-line)))

  ;; Please note flyspell only use ispell-word
  ;; (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  ;;   (let ((run-spellcheck ad-return-value))
  ;;     (if ad-return-value
  ;;       (cond
  ;;        ((org-mode-is-code-snippet)
  ;;         (setq run-spellcheck nil))
  ;;        ((org-mode-current-line-is-property)
  ;;         (setq run-spellcheck nil))))
  ;;     (setq ad-return-value run-spellcheck)))
  ;; }}

  ;; Org v8 change log:
  ;; @see http://orgmode.org/worg/org-8.0.html

  ;; {{ export org-mode in Chinese into PDF
  ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
  ;; and you need install texlive-xetex on different platforms
  ;; To install texlive-xetex:
  ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
  (setq org-latex-to-pdf-process ;; org v7
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-pdf-process org-latex-to-pdf-process) ;; org v8
  ;; }}

  ;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
  (defun narrow-or-widen-dwim ()
    "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
    (interactive)
    (cond ((buffer-narrowed-p) (widen))
          ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
          ((equal major-mode 'org-mode) (org-narrow-to-subtree))
          (t (error "Please select a region to narrow to"))))

  ;; Various preferences
  (setq org-log-done t
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-agenda-start-on-weekday nil
        org-agenda-span 14
        org-agenda-include-diary nil
        org-agenda-window-setup 'current-window
        org-fast-tag-selection-single-key 'expert
        org-export-kill-product-buffer-when-displayed t
        org-export-headline-levels 10
        ;; org v7
        org-export-odt-preferred-output-format "doc"
        ;; org v8
        org-odt-preferred-output-format "doc"
        org-tags-column 80
        ;; org-startup-indented t
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup t ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;; }}
        )

  ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))
  ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
  (setq org-outline-path-complete-in-steps t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

  (eval-after-load 'org-clock
    '(progn
       (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
       (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

  (eval-after-load 'org
    '(progn
       (setq org-imenu-depth 9)
       (require 'org-clock)
       ;; @see http://irreal.org/blog/1
       (setq org-src-fontify-natively t)))

  ;; 插入时间时, 显示到分钟
  ;; (defun rw-org-time-stamp ()
  ;;   (interactive)
  ;;   (org-time-stamp t))


  (defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
    (let ((browse-url-browser-function
           (cond ((equal (ad-get-arg 0) '(4))
                  'browse-url-generic)
                 ((equal (ad-get-arg 0) '(16))
                  'choose-browser)
                 (t
                  (lambda (url &optional new)
                    (w3m-browse-url url t))))))
      ad-do-it))

  (defadvice org-publish (around org-publish-advice activate)
    "Stop running major-mode hook when org-publish"
    (let ((old load-user-customized-major-mode-hook))
      (setq load-user-customized-major-mode-hook nil)
      ad-do-it
      (setq load-user-customized-major-mode-hook old)))

  ;;------------------------------------------------------------------------------
  ;; `org-entities' contain many useful symbols! C-c C-x \ toggle this feature.
  ;; use `org-entities-help' to see all symbols.
  ;;------------------------------------------------------------------------------

  (defun org-entities-find-utf (symbol)
    "find a utf-8 symbol in `org-entities'"
    (cl-loop for item in org-entities
             when (listp item)
             do (message "%s" (nth 6 item))
             and when (string= symbol (nth 6 item)) return item   
             finally (return nil)
             ))

  ;; allow one character to represent plain ordered list
  (setq org-list-allow-alphabetical t)

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (latex . t)
     (shell . t)
     ))

  ;; show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)

  ;; For languages with significant whitespace like Python, but I don't need it in coq.
  ;; (setq org-src-preserve-indentation t)

  ;;------------------------------------------------------------------------------
  ;; Toggle display of entities as UTF-8 characters
  ;; or #+STARTUP entitiespretty
  ;; characters are in `org-entities'
  ;;------------------------------------------------------------------------------
  ;; (setq org-pretty-entities t)


  ;;------------------------------------------------------------------------------
  ;; setting up capture
  ;;------------------------------------------------------------------------------
  (setq org-default-notes-file (expand-file-name "notes.org" "~/org/task"))
  ;; using org-capture to add ad hoc thinkings to note.org
  (define-key global-map (kbd "C-c c") 'org-capture)

  
  ;;------------------------------------------------------------------------------
  ;; 农历日期, from https://emacs-china.org/t/topic/2119/26
  ;; 例子: <%%(my-diary-chinese-anniversary 7 14)>
  ;;------------------------------------------------------------------------------
  
  (use-package cal-china
    :config
    ;; 原package里的这个函数, 有一个不知道干什么的, 从哪里设置的外部变量`date', 这代码也写的太垃圾了. 我也不想debug了, 自己写算了.
    ;; 我准备用`diary-anniversary'这个函数来实现, 设计一个将农历转化成阳历的函数.
    ;; 农历的标准格式中的"年"是由cycle*60+year来表示的, 比如, 公立1993对应的是农历第78个周期的第10年, 如果从公元前2637年算起的话.
    ;; 太麻烦了, 我找了一个能够成功起作用的了.
    ;; 只要不加year, 就是能起作用的.
    
    (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
      (if year
          (let* ((d-date (diary-make-date lunar-month lunar-day year))
                 (a-date (calendar-absolute-from-gregorian d-date))
                 (c-date (calendar-chinese-from-absolute a-date))
                 (cycle (car c-date))
                 (yy (cadr c-date))
                 (y (+ (* 100 cycle) yy)))
            (diary-chinese-anniversary lunar-month lunar-day y mark))
        (diary-chinese-anniversary lunar-month lunar-day year mark)))
    )
  
  ;; hook 
  (defun org-mode-hook-setup ()
    (setq truncate-lines nil)
    (setq word-wrap t)
    ;; added by rice-wine
    ;; (rainbow-delimiters-mode)
    (yas-minor-mode)
    ;; company
    (setup-company-mode '((
                           ;; 补全太慢了
                           ;; company-tabnine
                           company-capf
                           company-math-symbols-unicode)
                          ))

    ;; 插入时间时, 显示到分钟
    ;; (define-key org-mode-map [remap org-time-stamp] 'rw-org-time-stamp)
    )
  
  (add-hook 'org-mode-hook 'org-mode-hook-setup))

(provide 'init-org-mode)

