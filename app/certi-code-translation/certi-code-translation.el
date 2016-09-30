;;------------------------------------------------------------------------------
;; Convert c code to (c + primitive) code in coq notation,
;; used in our coq project.
;;------------------------------------------------------------------------------
(require 's)

;; (defun rw-certi-paren-encompass (s)
;;   "encompass string S with parens"
;;   (s-concat "(" s ")"))
(setq case-fold-search nil) ;; not ignore case 

(defvar rw-certi-reg-exp-var "\\(?:E[0-9]*\\)")
(defvar rw-certi-reg-s-var "\\(?:S[0-9]*\\)")

(defun rw-certi-syntax-exp-var? (s)
  (s-matches? (s-concat "^" rw-certi-reg-exp-var "$")
              s))

(defun rw-certi-syntax-s-var? (s)
  (s-matches? (s-concat "^" rw-certi-reg-s-var "$")
              s))

(rw-certi-syntax-s-var? "S111")
(rw-certi-syntax-exp-var? "E")

(defmacro rw-certi-parse-my-bnf (sexp)
  "Sub statements substitution:
translate \"(A)*(B)\" to \"(#{(rw-certi-translate-exp A)})*(#{(rw-certi-translate-exp B)})\"
But when symbol is begins as S, it means call `rw-certi-translate-s'."
  (let ((var-reg (s-concat
                  rw-certi-reg-exp-var
                  "\\|"
                  rw-certi-reg-s-var)))
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
                            (t
                             (message "Find an error syntax variable: %s!" sub))))
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

(defun rw-certi-translate-exp (exp)
  (pcase exp
    ;; enull
    (`null "NULL")
    ;; symbol
    ((and (pred symbolp) sym)
     (symbol-name sym))
    ;; integer
    ((and (pred integerp) i)
     (s-concat "′" (int-to-string i)))
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
    (`(cast ,E1 ,E2)
     (rw-certi-parse-my-bnf "〈E1〉(E2)"))
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
     (rw-certi-parse-my-bnf "∼E"))
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
    (_
     (format "parse %s error!" exp))
    ))


(rw-certi-translate-exp '(cast (var OSInt) Int32))
(rw-certi-translate-exp '(aref (var OSInt) 10))
(rw-certi-translate-exp '(decf (var OSIntNesting)))
(rw-certi-translate-exp '(~ 10))
(rw-certi-translate-exp '(evar x))
(rw-certi-translate-exp
 '(cast TASK_ID
        (f-> (f. (aref (var OSTaskRdyList) (var prio))
                 Head)
             Node)))

(setq rw-certi-s-parse-list
      `(;; assign
        (`(setq ,E1 ,E2)
         "E1 =ₑ E2")
        ;; &=
        (`(setq ,E1 (bitand ,E1 ,E2))
         "E1 &= E2")
        ;; if ... then ... else ...
        (`(if ,E ,S1 ,S2)
         "IF (E) { S1 } ELSE { S2 }")
        ;; if ... then ...
        (`(if ,E ,S)
         "If (E) { S }")
        ;; while
        (`(while ,E ,S)
         "WHILE (E) { S }")
        ;; other
        (w
         'literal
         (message "Parse rule for '%s' is not defined!" w))
        ))

(-map (lambda (item)
        (pcase item
          (`(,pattern literal ,clause)
           ;; clause is literal
           `(,pattern ,clause))
          (`(,pattern ,clause)
           ;; clause need parsing
           `(,pattern
             (rw-certi-parse-my-bnf ,clause))
           )))
      '((A B)
        (A2 B2)))

(defmacro rw-certi-unfold-pcase-list (s parse-list)
  (let ((parse-list-1 (gensym)))
    `(let ((,parse-list-1 
            (-map (lambda (item)
                    (pcase item
                      (`(,pattern literal ,clause)
                       ;; clause is literal
                       `(,pattern ,clause))
                      (`(,pattern ,clause)
                       ;; clause need parsing
                       `(,pattern
                         (rw-certi-parse-my-bnf ,clause))
                       )))
                  ,parse-list)))
       (pcase ,s
         ,@,parse-list-1))))

(macroexpand '(rw-certi-unfold-pcase-list s rw-certi-s-parse-list))

(defun rw-certi-translate-s (s)
  "Translate statement."
  (rw-certi-unfold-pcase-list s rw-certi-s-parse-list))



(defun rw-certi-translate-declare-list (ds)
  (pcase ds
    (`((,I ,T) . ,tail)
     `(dcons ,I ,T
             ,(rw-certi-translate-declare-list tail)))
    (`nil 'dnil)))

(rw-certi-translate-declare-list
 '((x int32)
   (y int32)))

(defun rw-certi-translate-sseq (ss)
  (pcase ss
    (`(,s)
     )
    (`(,s1 ,s2)
     `(sseq ,s1 ,s2))
    (`(,s . ,tail)
     `(sseq ,s
            ,(rw-certi-translate-sseq tail)))))


(defun_cfunc OSTaskReSched ()
  (declare_list
   (tasktmp . TASK_ID)
   (prio . Int32))
  (if (<= OSTaskLockCnt 0)
      (progn
        (setq prio (GetHighestTaskPrio))
        (setq tasktmp
              (cast TASK_ID
                    (efieldp
                     (efield
                      (earray OSTaskRdyList prio)
                      Head)
                     Node)))
        (if (<> tasktmp OSTaskCur)
            (progn
              (setq OSTaskNew tasktmp)
              (setq OSTaskSwitchFlag OSTRUE)))
        (return))))

