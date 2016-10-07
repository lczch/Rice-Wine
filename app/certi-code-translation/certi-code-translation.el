;;------------------------------------------------------------------------------
;; Convert c code to (c + primitive) code in coq notation,
;; used in our coq project.
;;------------------------------------------------------------------------------
(require 's)

(setq case-fold-search nil) ;; not ignore case 

(defvar rw-certi-reg-exp-var "\\(?:\\bE[0-9]*\\b\\)")
(defvar rw-certi-reg-s-var "\\(?:\\bS[0-9]*\\b\\)")
(defvar rw-certi-reg-type-var "\\(?:\\bT[0-9]*\\b\\)")

(defun rw-certi-syntax-exp-var? (s)
  (s-matches? (s-concat "^" rw-certi-reg-exp-var "$")
              s))

(defun rw-certi-syntax-s-var? (s)
  (s-matches? (s-concat "^" rw-certi-reg-s-var "$")
              s))

(defun rw-certi-syntax-type-var? (s)
  (s-matches? (s-concat "^" rw-certi-reg-type-var "$")
              s))

(rw-certi-syntax-s-var? "S13")
(rw-certi-syntax-exp-var? "E101010")
(rw-certi-syntax-type-var? "T")

(defmacro rw-certi-parse-my-bnf (sexp)
  "Sub statements substitution:
translate \"(A)*(B)\" to \"(#{(rw-certi-translate-exp A)})*(#{(rw-certi-translate-exp B)})\"
But when symbol is begins as S, it means call `rw-certi-translate-s'."
  (let ((var-reg (s-concat
                  rw-certi-reg-exp-var
                  "\\|"
                  rw-certi-reg-s-var
                  "\\|"
                  rw-certi-reg-type-var)))
    `(-reduce #'s-concat
              (-map #'(lambda (s)
                        (pcase (s-match ,var-reg s)
                          (`(,sub)
                           (cond
                            ((rw-certi-syntax-exp-var? sub)
                             ;; is a Exp var
                             (s-replace sub
                                        (rw-certi-translate-exp (symbol-value (intern-soft sub)))
                                        s))
                            ((rw-certi-syntax-s-var? sub)
                             ;; is a Statement var
                             (s-replace sub
                                        (rw-certi-translate-s (symbol-value (intern-soft sub)))
                                        s))
                            ((rw-certi-syntax-type-var? sub)
                             ;; is a Statement var
                             (s-replace sub
                                        (rw-certi-translate-type (symbol-value (intern-soft sub)))
                                        s))
                            (t
                             (error "Find an error syntax variable: %s!" sub))))
                          (`nil
                           ;; do not have syntax variables
                           s)))
                    (s-slice-at ,var-reg ,sexp)))))

;; (defmacro rw-certi-syntax-exp (sexp)
;;   "Sub expression substitution:
;; translate \"(A)*(B)\" to \"(#{(rw-certi-translate-exp A)})*(#{(rw-certi-translate-exp B)})\""
;;   `(-reduce #'s-concat
;;             (-map #'(lambda (s)
;;                       (pcase (s-match "[A-Z]" s)
;;                         (`(,sub)
;;                          (s-replace sub
;;                                     (rw-certi-translate-exp (symbol-value (intern-soft sub)))
;;                                     s))
;;                         (`nil
;;                          s)))
;;                   (s-slice-at "[A-Z]" ,sexp))))

;; (setq foo 1)
;; (setq sym (intern "foo"))
;; (symbol-value sym)
;; (intern-soft "foo")
;; (unintern "foo")

;; (car (s-match "[A-Z]" "A[B]"))
;; (macroexpand '(rw-certi-syntax-exp "A[B]"))

(defun rw-certi-translate-type (type)
  (pcase type
    ;; void
    (`void
     "Void")
    (`int8
     "Int8u")
    (`int16
     "Int16u")
    (`int32
     "Int32u")
    (`ptr
     "Ptr")
    (`(ptr ,T)
     (s-concat (rw-certi-translate-type T)
               " ∗"))
    ((and sym (pred symbolp))
     (symbol-name sym))
    (_
     (error "Parsing Type error: %s" type))
    ))

(rw-certi-translate-type 'TaskID)

(defun rw-certi-translate-exp (exp)
  (pcase exp
    ;; enull
    (`null "NULL")
    ;; evar
    (`(var ,E)
     (rw-certi-parse-my-bnf "E′"))
    ;; ederef
    (`(p* ,E)
     (rw-certi-parse-my-bnf "∗(E)"))
    ;; eaddrof
    (`(p& ,E)
     (rw-certi-parse-my-bnf "&ₐ(E)"))
    ;; ecast
    (`(cast ,T ,E)
     (rw-certi-parse-my-bnf "〈T〉(E)"))
    ;; earrayelem
    (`(aref ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)[E2]"))
    ;; efield point
    (`(f-> ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)→E2"))
    ;; efield
    (`(f. ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)·ₑE2"))
    ;; negation
    (`(~ ,E)
     (rw-certi-parse-my-bnf "∼(E)"))
    ;; minus
    (`(- ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)−(E2)"))
    ;; plus
    (`(+ ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)+ₑ(E2)"))
    ;; incf
    (`(incf ,E)
     (rw-certi-parse-my-bnf "++(E)"))
    ;; decf
    (`(decf ,E)
     (rw-certi-parse-my-bnf "--(E)"))
    ;; multi
    (`(* ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)×(E2)"))
    ;; div
    (`(/ ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)÷(E2)"))
    ;; shiftl
    (`(lsh ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)≪(E2)"))
    ;; shiftr
    (`(rsh ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)≫(E2)"))
    ;; and
    (`(and ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)&&ₑ(E2)"))
    ;; or
    (`(or ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)||ₑ(E2)"))
    ;; logand
    (`(bitand ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)&ₑ(E2)"))
    ;; logor
    (`(bitor ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)|ₑ(E2)"))
    ;; equal
    (`(= ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)==ₑ(E2)"))
    ;; neq
    (`(!= ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)!=ₑ(E2)"))
    ;; less
    (`(< ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)<ₑ(E2)"))
    ;; greater
    (`(> ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)>ₑ(E2)"))
    ;; less-or-equal
    (`(<= ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)≤(E2)"))
    ;; greater-or-equal
    (`(>= ,E1 ,E2)
     (rw-certi-parse-my-bnf "(E1)≥(E2)"))
    ;; symbol
    ((and (pred symbolp) sym)
     (symbol-name sym))
    ;; integer
    ((and (pred integerp) i)
     (s-concat "′" (int-to-string i)))
    (_
     (format "parse %s error!" exp))
    ))


(rw-certi-translate-exp '(cast int32 (var OSInt)))
(rw-certi-translate-exp '(aref (var OSInt) 10))
(rw-certi-translate-exp '(decf (var OSIntNesting)))
(rw-certi-translate-exp '(~ 10))
(rw-certi-translate-exp '(evar x))
(rw-certi-translate-exp
 '(cast TASK_ID
        (f-> (f. (aref (var OSTaskRdyList) (var prio))
                 Head)
             Node)))

;; there is not &body keywords ....
;; (defmacro def-rw-certi-parser (parser-name pname &rest parse-list)
;;   (let* ((pname-1 (pcase pname
;;                     (`(,item) item)))
;;          (parse-list-1 
;;           (-map (lambda (item)
;;                   (pcase item
;;                     (`(,pattern literal ,clause)
;;                      ;; clause is literal
;;                      `(,pattern ,clause))
;;                     (`(,pattern ,clause)
;;                      ;; clause need parsing
;;                      `(,pattern
;;                        (rw-certi-parse-my-bnf ,clause))
;;                      )))
;;                 parse-list)))
;;     `(defun ,parser-name ,pname
;;        (pcase ,pname-1
;;          ,@parse-list-1))))

(let ((l '(3)))
  `(1 2 ,@l))

(defun rw-certi--add-spliter (spliter s-list)
  "Add spliter on last element."
  (--update-at (1- (length s-list))
               (pcase it
                 (`(,depth ,str)
                  `(,depth ,(s-concat str spliter))))
               s-list))

(rw-certi--add-spliter
 ";ₛ"
 '((1 "hehe")
   (1 "haha")
   (3 "mem")))

(rw-certi--add-spliter
 ";ₛ"
 '((1 "hehe")))

(defun rw-certi--make-record (depth print-data)
  (if (and (integerp depth) (stringp print-data))
      (list depth print-data)
    (error "rw-certi-make-record error: (%s %s) is wrong arguments." depth print-data))
  )

(rw-certi--make-record 1 "12")

(defun rw-certi--record-get-depth (record)
  (first record))

(defun rw-certi--record-get-data (record)
  (second record))

(defun rw-certi--record-update-data (record data)
  (rw-certi--make-record
   (rw-certi--record-get-depth record)
   data))

(defun rw-certi--record-update-data-f (record fn)
  (rw-certi--record-update-data
   record
   (funcall fn (rw-certi--record-get-data record))))

(defun rw-certi--record-add-spliter (record spliter)
  (rw-certi--record-update-data-f
   record
   #'(lambda (s)
       (s-concat s spliter))))

(defun rw-certi--make-records (&rest records)
  "records is list of record"
  records)

(rw-certi--make-records
 (rw-certi--make-record 1 "12")
 (rw-certi--make-record 3 "AA"))

(defun rw-certi--records-merge (rs1 rs2)
  (-concat rs1 rs2))

(defun rw-certi--records-add-spliter (records spliter)
  "Add spliter to last item of records."
  (--update-at (1- (length records))
               (rw-certi--record-add-spliter it spliter)
               records))

(rw-certi--records-add-spliter '((1 "2")
                                 (3 "4"))
                               ";")

;; (defun -map-indexed-when (pred rep list)
;;   "indexed version of `-map-when', PRED and REP both accept two parameters: (index item)."
;;   (->> list
;;        (-map-indexed #'list)
;;        (-map #'(lambda (item)
;;                  (if (apply pred item)
;;                      (apply rep item)
;;                    (second item)))
;;              )))

;; (-map-indexed-when
;;  #'(lambda (i x)
;;      (evenp i))
;;  #'(lambda (i x)
;;      (1+ x))
;;  '(1 2 3 4 5))

(-map-indexed #'list '(10 20 30))

(defun rw-certi-translate-s (s &optional depth)
  (let ((depth (or depth 0)))
    (pcase s
      ;; &=
      (`(setq ,E1
              (bitand ,(and E11
                            (pred (equal E1)))
                      ,E2))
       `((,depth ,(rw-certi-parse-my-bnf "E1 &= E2"))))
      ;; if ... then ... else ...
      (`(if ,E ,S1 ,S2)
       `((,depth ,(rw-certi-parse-my-bnf "IF (E)"))
         (,depth "{")
         ,@(rw-certi-translate-s S1 (1+ depth))
         (,depth "}")
         (,depth "ELSE")
         (,depth "{")
         ,@(rw-certi-translate-s S2 (1+ depth))
         (,depth "}")))
      ;; if ... then ...
      (`(if ,E ,S)
       `((,depth ,(rw-certi-parse-my-bnf "If (E)"))
         (,depth "{")
         ,@(rw-certi-translate-s S (1+ depth))
         (,depth "}"))
       )
      ;; while
      (`(while ,E ,S)
       `((,depth ,(rw-certi-parse-my-bnf "WHILE (E)"))
         (,depth "{")
         ,@(rw-certi-translate-s S (1+ depth))
         (,depth "}"))
       )
      ;; return
      (`(return)
       `((,depth "RETURN")))
      ;; return e
      (`(return ,E)
       `((,depth ,(rw-certi-parse-my-bnf "RETURN E")))
       )
      ;; funcall
      (`(funcall ,fname . ,paras)
       (let* ((s-paras
               (if paras
                   ;; paras is not nil
                   (->> paras
                        (-map #'rw-certi-translate-exp)
                        (-reduce #'(lambda (a b)
                                     (s-concat a " , " b)))
                        ((lambda (s)
                           (s-concat "(­" s "-)"))))
                 "(­)")))
         `((,depth ,(s-concat (symbol-name fname)
                              s-paras)))))
      ;; funcall with return value
      (`(setq ,E (funcall ,fname . ,paras))
       (let* ((s-paras
               (if paras
                   ;; paras is not nil
                   (->> paras
                        (-map #'rw-certi-translate-exp)
                        (-reduce #'(lambda (a b)
                                     (s-concat a " , " b)))
                        ((lambda (s)
                           (s-concat "(·" s "·)"))))
                 "(·)")))
         `((,depth ,(s-concat
                     (rw-certi-parse-my-bnf "E =ᶠ ")
                     (symbol-name fname)
                     s-paras)))))
      ;; sequence
      (`(progn . ,stats)
       (->> stats
            (--map (rw-certi-translate-s it depth))
            (--reduce-r (rw-certi--records-merge
                         (rw-certi--records-add-spliter it ";ₛ")
                         acc))
            ))
      ;; assign 
      (`(setq ,E1 ,E2)
       `((,depth ,(rw-certi-parse-my-bnf "E1 =ₑ E2"))))
      ;; primitivs:
      ;; exint
      (`(exint)
       `((,depth "IRET")))
      ;; excrit
      (`(excrit)
       `((,depth "OSIntUnlock(intlevel)")))
      ;; encrit
      (`(encrit)
       `((,depth "intlevel=OSIntLock()")))
      ;; switch
      (`(switch)
       `((,depth "SWITCH")))
      ;; sti
      (`(sti)
       `((,depth "Enable_Interrupt()")))
      ;; cli
      (`(cli)
       `((,depth "Disable_Interrupt()")))
      ;; checkis
      (`(checkis ,x)
       `((,depth ,(s-concat
                   (symbol-name x)
                   " =OSGetIntNesting()"))))
      ;; other
      (w
       (error "Parse rule for statement '%s' is not defined!" w)))))


(-concat '((1) (2)) '((3)))
(rw-certi-translate-s '(setq (var x)
                             (bitand (var x) 10)))
(rw-certi-translate-s '(if 1
                           (setq (var x) 10)
                         (setq (var y) 20)))
(rw-certi-translate-s '(if 1 (setq (var x) 29)))
(rw-certi-translate-s '(return 10))
(rw-certi-translate-s '(funcall hehe (var x) (var y) 2))
(rw-certi-translate-s '(funcall OSTaskReSched 1))
(rw-certi-translate-s '(setq (p* (var x))
                             (funcall hehe (var y) 2)))
(rw-certi-translate-s '(if (= (var x) 1)
                           (progn
                             (setq (p* (var x))
                                   (funcall hehe (var y) 2))
                             (setq (var x) 3))))

(rw-certi-translate-s '(checkis nest))

(defvar rw-certi-tab (s-repeat 4 " "))

(defun rw-certi-indent (depth)
  (s-repeat depth rw-certi-tab))

(defun rw-certi--print-record (record)
  (let ((depth (rw-certi--record-get-depth record))
        (data (rw-certi--record-get-data record)))
    (insert (rw-certi-indent depth)
            data "\n")))

(defun rw-certi--print-records (records)
  (-each records
    #'rw-certi--print-record))

(defun rw-certi-print-s (s)
  (rw-certi--print-records
   (rw-certi-translate-s s)))

(rw-certi-print-s '(funcall OSTaskReSched))

(rw-certi-print-s '(progn
                     (if (= (var x) 1)
                         (progn
                           (setq (p* (var x))
                                 (funcall hehe (var y) 2))
                           (setq (var x) 3)))
                     (setq (var y) (var x))))


(rw-certi-print-s
 '(if (<= OSTaskLockCnt 0)
      (setq (var x) 10)
    (setq (var y) 20)))IF ((OSTaskLockCnt)≤(′0))


(defun rw-certi-translate-decl (dl depth)
  (pcase dl
    (`(,sym ,T)
     (rw-certi--make-record depth
                           (s-concat (symbol-name sym)
                                     " @ "
                                     (rw-certi-translate-type T))))
    (_
     (error "Parsing declare %s error!" dl))))

(rw-certi-translate-decl '(hehe int8) 1)

(defun rw-certi-translate-decls (dls &optional depth)
  (let ((depth (or depth 1)))
    (pcase dls
      (`(declare . ,dcls)
       (if (not dcls)
           (rw-certi--make-records
            (rw-certi--make-record depth "⌞")
            (rw-certi--make-record depth "⌟"))
         (let ((dcls-1
                (->> dcls
                     (-map #'(lambda (it)
                                (rw-certi--make-records
                                 (rw-certi-translate-decl it (1+ depth)))))
                     (--reduce-r (rw-certi--records-merge
                                  (rw-certi--records-add-spliter it ";") acc)))))
           `(,(rw-certi--make-record depth "⌞")
             ,@dcls-1
             ,(rw-certi--make-record depth "⌟"))))))))

(rw-certi-translate-decls
 `(declare
   (task int8)
   (prio int32)
   (tasktmp TASK_ID)))

(rw-certi-translate-decls
 `(declare
   (task int8)))

(defun rw-certi--print-record-compact (record)
  (let ((data (rw-certi--record-get-data record)))
    (insert data " ")))

(defun rw-certi--print-records-compact (records)
  (-each records
    #'rw-certi--print-record-compact))

(defun rw-certi-print-func (func)
  (pcase func
    (`(defunc ,fun-type ,fun-name
        ,para-decls
        ,local-decls
        ,body)
     ;; print type and name
     (insert (rw-certi-translate-type fun-type)
             " ·" (symbol-name fun-name) "·")
     (insert "(")
     ;; print parameter decls
     (rw-certi--print-records-compact
      (rw-certi-translate-decls para-decls))
     (insert ")··{\n")
     ;; print local decls
     (rw-certi--print-records
      (rw-certi--records-add-spliter
       (rw-certi-translate-decls local-decls 1)
       ";"))
     ;; print statements
     (rw-certi--print-records
      (rw-certi-translate-s body 1))
     ;; end
     (insert "}·.\n"))
    ))

(rw-certi-print-func
 '(defunc void OSTaskReSched
    (declare)
    (declare
     (tasktmp TASK_ID)
     (prio int32))
    (if (<= (var OSTaskLockCnt) 0)
        (progn
          (setq (var prio) (funcall GetHighestTaskPrio))
          (setq (var tasktmp)
                (cast TASK_ID
                      (f->
                       (f. (aref (var OSTaskRdyList) (var prio))
                           Head)
                       Node)))
          (if (!= (var tasktmp) (var OSTaskCur))
              (progn
                (setq (var OSTaskNew) (var tasktmp))
                (setq (var OSTaskSwitchFlag) OSTRUE))
            (return))))))

;; (defun_cfunc OSTaskReSched ()
;;   (declare_list
;;    (tasktmp . TASK_ID)
;;    (prio . Int32))
;;   (if (<= OSTaskLockCnt 0)
;;       (progn
;;         (setq prio (GetHighestTaskPrio))
;;         (setq tasktmp
;;               (cast TASK_ID
;;                     (efieldp
;;                      (efield
;;                       (earray OSTaskRdyList prio)
;;                       Head)
;;                      Node)))
;;         (if (<> tasktmp OSTaskCur)
;;             (progn
;;               (setq OSTaskNew tasktmp)
;;               (setq OSTaskSwitchFlag OSTRUE)))
;;         (return))))

