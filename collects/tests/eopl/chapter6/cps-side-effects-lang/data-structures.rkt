#lang eopl

(require "cps-out-lang.rkt")          ; for tfexp?
(require "store.rkt")                 ; for reference?

(provide (all-defined-out))           ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (ref-val
   (ref reference?))
  )

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) ref)
      (else (expval-extractor-error 'reference v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

;; the interpreter is tail-recursive, so it really doesn't do
;; anything with the continuation.  So all we need is one
;; continuation value.

(define-datatype continuation continuation?
  (end-cont)                     
  )

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (vars (list-of symbol?))
   (body tfexp?)
   (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;;; represent environment as a list of bindings.
;;; binding ::= ('let    (list-of id) (list-of expval)) 
;;;           | ('letrec (list-of id) (list-of bvar) (list-of tfexp))

;;; The first binding for extend-env, the second is for
;;; extend-env-rec**. 

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

(define empty-env
  (lambda ()
    '()))

(define empty-env? 
  (lambda (x) (null? x)))

(define extend-env*
  (lambda (syms vals old-env)
    (cons (list 'let syms vals) old-env)))

(define extend-env-rec**
  (lambda (p-names b-varss p-bodies saved-env)
    (cons 
     (list 'letrec p-names b-varss p-bodies)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env) 
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (let ((pos (list-index search-sym (cadr binding))))
            (if pos
                (case (car binding)
                  ((let)
                   (list-ref (caddr binding) pos))
                  ((letrec)
                   (let ((bvars (caddr binding))
                         (bodies (cadddr binding)))
                     (proc-val
                      (procedure
                       (list-ref bvars pos)
                       (list-ref bodies pos)
                       env)))))
                (apply-env saved-env search-sym)))))))

;; returns position of sym in los, else #f
(define list-index
  (lambda (sym los)
    (let loop ((pos 0) (los los))
      ;; los is at position pos of the original los
      (cond
        ((null? los) #f)
        ((eqv? sym (car los)) pos)
        (else (loop (+ pos 1) (cdr los)))))))

;; not precise, but will do.
(define environment?
  (list-of
   (lambda (p)
     (and 
      (pair? p)
      (or (eqv? (car p) 'let) (eqv? (car p) 'letrec))))))


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> environment

;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.  

(define init-env 
  (let ((extend-env1
         (lambda (sym val env)
           (extend-env* (list sym) (list val) env))))    
    (lambda ()
      (extend-env1
       'i (num-val 1)
       (extend-env1
        'v (num-val 5)
        (extend-env1
         'x (num-val 10)
         (empty-env)))))))

;; exercise:  Improve this code by getting rid of extend-env1.
