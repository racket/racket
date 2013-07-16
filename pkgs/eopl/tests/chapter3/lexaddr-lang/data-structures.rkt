#lang eopl

;; data structures for LEXADDR language

(require "lang.rkt")                  ; for expression?

(provide (all-defined-out))           ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?)))

;;; extractors:

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;


;; proc? : SchemeVal -> Bool
;; procedure : Exp * Nameless-env -> Proc
(define-datatype proc proc?
  (procedure
   ;; in LEXADDR, bound variables are replaced by %nameless-vars, so
   ;; there is no need to declare bound variables.
   ;; (bvar symbol?)
   (body expression?)
   ;; and the closure contains a nameless environment
   (env nameless-environment?)))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;; nameless-environment? : SchemeVal -> Bool
;; Page: 99
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env : () -> Nameless-env
;; Page: 99
(define empty-nameless-env
  (lambda ()
    '()))

;; empty-nameless-env? : Nameless-env -> Bool
(define empty-nameless-env? 
  (lambda (x)
    (null? x)))

;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
;; Page: 99
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
;; Page: 99
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))
