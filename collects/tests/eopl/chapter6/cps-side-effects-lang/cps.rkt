#lang eopl

(require "cps-in-lang.rkt")
(require "cps-out-lang.rkt")

(provide cps-of-program)

;; cps-of-program : InpExp -> TfExp
;; Page: 224
(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (cps-a-program
                  (cps-of-exps (list exp1)
                               (lambda (new-args) 
                                 (simple-exp->exp (car new-args)))))))))

;; cps-of-exp : Exp * SimpleExp -> TfExp
;; Page: 222, 228, 231
(define cps-of-exp
  (lambda (exp k-exp)
    (cases expression exp
      (const-exp (num) (make-send-to-cont k-exp (cps-const-exp num)))
      (var-exp (var) (make-send-to-cont k-exp (cps-var-exp var)))
      (proc-exp (vars body) 
                (make-send-to-cont k-exp
                                   (cps-proc-exp (append vars (list 'k%00))
                                                 (cps-of-exp body (cps-var-exp 'k%00)))))
      (zero?-exp (exp1)
                 (cps-of-zero?-exp exp1 k-exp))
      (diff-exp (exp1 exp2)
                (cps-of-diff-exp exp1 exp2 k-exp))
      (sum-exp (exps)
               (cps-of-sum-exp exps k-exp))
      (if-exp (exp1 exp2 exp3)
              (cps-of-if-exp exp1 exp2 exp3 k-exp))
      (let-exp (var exp1 body)
               (cps-of-let-exp var exp1 body k-exp))
      (letrec-exp (ids bidss proc-bodies body)
                  (cps-of-letrec-exp ids bidss proc-bodies body k-exp))
      (call-exp (rator rands)
                (cps-of-call-exp rator rands k-exp))
      
      ;; new for cps-side-effects-lang
      ;; Page: 228
      (print-exp (rator)
                 (cps-of-exps (list rator)
                              (lambda (simples)
                                (cps-printk-exp
                                 (car simples)
                                 (make-send-to-cont k-exp (cps-const-exp 38))))))
      
      ;; Page 231
      (newref-exp (exp1)
                  (cps-of-exps (list exp1)
                               (lambda (simples)
                                 (cps-newrefk-exp (car simples) k-exp))))
      
      (deref-exp (exp1)
                 (cps-of-exps (list exp1)
                              (lambda (simples)
                                (cps-derefk-exp (car simples) k-exp))))
      
      (setref-exp (exp1 exp2)
                  (cps-of-exps (list exp1 exp2)
                               (lambda (simples)
                                 (cps-setrefk-exp 
                                  (car simples)
                                  (cadr simples)
                                  ;; the third argument will be evaluated tail-recursively.
                                  ;; returns 23, just like in explicit-refs
                                  (make-send-to-cont k-exp (cps-const-exp 23))))))
      
      )))

;; cps-of-exps : (list-of expression) * 
;;                      ((list-of cps-simple-expression) -> cps-expression)
;;                      -> cps-expression
;; Page: 219
;; usage: 
;;   -- assume e_i's are non-simple, b_i's are simple
;;   -- then 
;;        (cps-of-exps '(b1 b2 e1 b3 e2 e3) F) ==
;;        [e1](\v1.[e2](\v2.[e3](\v3.(F `(,<b1> ,<b2> ,v1 ,<b3> ,v2 ,v3)))))
;;      where <b> is cps-of-simple-exp of b.
(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ((exps exps))
      ;; cps-of-rest : Listof(InpExp) -> TfExp
      (let ((pos (list-index
                  (lambda (exp)
                    (not (inp-exp-simple? exp)))
                  exps)))
        (if (not pos)
            (builder (map cps-of-simple-exp exps))
            (let ((var (fresh-identifier 'var)))
              (cps-of-exp
               (list-ref exps pos)
               (cps-proc-exp (list var)
                             (cps-of-rest
                              (list-set exps pos (var-exp var)))))))))))

;; inp-exp-simple? : InpExp -> Bool
;; returns #t or #f, depending on whether exp would be a 
;; simple-exp if reparsed using the CPS-OUT language.
(define inp-exp-simple?
  (lambda (exp)
    (cases expression exp
      (const-exp (num) #t)
      (var-exp (var) #t)
      (diff-exp (exp1 exp2)
                (and
                 (inp-exp-simple? exp1)
                 (inp-exp-simple? exp2)))
      (zero?-exp (exp1)
                 (inp-exp-simple? exp1))
      (proc-exp (ids exp) #t)
      (sum-exp (exps)
               (all-simple? exps))
      (else #f))))

(define all-simple?
  (lambda (exps)
    (if (null? exps)
        #t
        (and (inp-exp-simple? (car exps))
             (all-simple? (cdr exps))))))


;; takes a list of expressions and finds the position of the first
;; one that is not a simple-exp, else returns #f
(define index-of-first-non-simple
  (lambda (exps)
    (cond
      ((null? exps) #f)
      ((inp-exp-simple? (car exps))
       (let ((pos (index-of-first-non-simple (cdr exps))))
         (if pos
             (+ pos 1) #f)))
      (else 0))))

;; cps-of-simple-exp : InpExp -> SimpleExp
;; Page: 220
;; assumes (inp-exp-simple? exp).
(define cps-of-simple-exp
  (lambda (exp)
    (cases expression exp
      (const-exp (num) (cps-const-exp num))
      (var-exp (var) (cps-var-exp var))
      (diff-exp (exp1 exp2)
                (cps-diff-exp
                 (cps-of-simple-exp exp1)
                 (cps-of-simple-exp exp2)))
      (zero?-exp (exp1)
                 (cps-zero?-exp
                  (cps-of-simple-exp exp1)))
      (proc-exp (ids exp) 
                (cps-proc-exp (append ids (list 'k%00))
                              (cps-of-exp exp (cps-var-exp 'k%00))))
      (sum-exp (exps)
               (cps-sum-exp
                (map cps-of-simple-exp exps)))
      (else 
       (report-invalid-exp-to-cps-of-simple-exp exp)))))

(define report-invalid-exp-to-cps-of-simple-exp
  (lambda (exp)
    (eopl:error 'cps-simple-of-exp
                "non-simple expression to cps-of-simple-exp: ~s"
                exp)))

;; make-send-to-cont : SimpleExp * SimpleExp -> TfExp
;; Page: 214
(define make-send-to-cont
  (lambda (cont bexp)
    (cps-call-exp cont (list bexp))))


;; cps-of-zero?-exp : InpExp * SimpleExp -> TfExp
;; Page: 222
(define cps-of-zero?-exp
  (lambda (exp1 k-exp)
    (cps-of-exps (list exp1)
                 (lambda (new-rands)
                   (make-send-to-cont
                    k-exp
                    (cps-zero?-exp 
                     (car new-rands)))))))

;; cps-of-sum-exp : Listof (InpExp) * SimpleExp -> TfExp
;; Page: 219
(define cps-of-sum-exp
  (lambda (exps k-exp)
    (cps-of-exps exps
                 (lambda (new-rands)
                   (make-send-to-cont
                    k-exp
                    (cps-sum-exp new-rands))))))

;; cps-of-diff-exp : InpExp * InpExp * SimpleExp -> TfExp
;; Page: 223
(define cps-of-diff-exp
  (lambda (exp1 exp2 k-exp)
    (cps-of-exps
     (list exp1 exp2)
     (lambda (new-rands)
       (make-send-to-cont
        k-exp
        (cps-diff-exp
         (car new-rands)
         (cadr new-rands)))))))


;; cps-of-if-exp : InpExp * InpExp * InpExp * SimpleExp -> TfExp
;; Page: 223
(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 k-exp)
    (cps-of-exps (list exp1)
                 (lambda (new-rands)
                   (cps-if-exp (car new-rands)
                               (cps-of-exp exp2 k-exp)
                               (cps-of-exp exp3 k-exp))))))

;; cps-of-let-exp : Var * InpExp * InpExp * SimpleExp -> TfExp
;; Page: 222
(define cps-of-let-exp
  (lambda (id rhs body k-exp)
    (cps-of-exps (list rhs)
                 (lambda (new-rands)
                   (cps-let-exp id 
                                (car new-rands)
                                (cps-of-exp body k-exp))))))

;; cps-of-letrec-exp :
;; Listof(Listof(Var)) * Listof(InpExp) * InpExp * SimpleExp -> TfExp
;; Page: 223
(define cps-of-letrec-exp
  (lambda (proc-names idss proc-bodies body k-exp)
    (cps-letrec-exp
     proc-names
     (map
      (lambda (ids) (append ids (list 'k%00)))
      idss)
     (map
      (lambda (exp) (cps-of-exp exp (cps-var-exp 'k%00)))
      proc-bodies)
     (cps-of-exp body k-exp))))

;; cps-of-call-exp : InpExp * Listof(InpExp) * SimpleExp -> TfExp
;; Page: 220
(define cps-of-call-exp
  (lambda (rator rands k-exp)
    (cps-of-exps (cons rator rands)
                 (lambda (new-rands)
                   (cps-call-exp
                    (car new-rands)
                    (append (cdr new-rands) (list k-exp)))))))

;;;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;;;

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

;; list-set : SchemeList * Int * SchemeVal -> SchemeList
;; returns a list lst1 that is just like lst, except that 
;; (listref lst1 n) = val.
(define list-set
  (lambda (lst n val)
    (cond
      ((null? lst) (eopl:error 'list-set "ran off end"))
      ((zero? n) (cons val (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- n 1) val))))))

;; list-index : (SchemeVal -> Bool) * SchemeList -> Maybe(Int)
;; returns the smallest number n such that (pred (listref lst n))
;; is true.  If pred is false on every element of lst, then returns
;; #f. 
(define list-index
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) 0)
      ((list-index pred (cdr lst)) => (lambda (n) (+ n 1)))
      (else #f))))

