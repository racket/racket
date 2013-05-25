#lang eopl  

(require "data-structures.rkt")
(provide init-env empty-env extend-env extend-env-rec apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> environment

;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.  

(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;;; represent environment as an alist  ((id rhs) ...)

;;; rhs is either an expval or a list (bvar body)
;;; expval is for extend-env; the list is for extend-env-rec.

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

(define empty-env
  (lambda ()
    '()))

(define empty-env? 
  (lambda (x) (null? x)))

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extend-env-rec
  (lambda (p-name b-var p-body saved-env)
    (cons 
     (list p-name b-var p-body)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env) 
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (id (list-ref binding 0))
               (expval-or-bvar (list-ref binding 1)))
          (cond
            ((not (eqv? search-sym id))
             (apply-env (cdr env) search-sym))
            ((not (symbol? expval-or-bvar))
             ;; this was built by extend-env
             expval-or-bvar)
            (else
             ;; this was built by extend-env-rec
             (let ((bvar (cadr binding))
                   (body (caddr binding)))
               (proc-val (procedure bvar body env)))))))))
