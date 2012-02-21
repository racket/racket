#lang eopl
(require "lang.rkt")                  ; for expression?
(require "store.rkt")
;; (provide (all-from "lang.rkt"))
(provide (all-defined-out))           ; too many things to list


;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (list-val
   (lst (list-of expval?)))
  (mutex-val
   (mutex mutex?))
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

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v)))))

(define expval->mutex
  (lambda (v)
    (cases expval v
      (mutex-val (l) l)
      (else (expval-extractor-error 'mutex v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; mutexes ;;;;;;;;;;;;;;;;

(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed?    reference?)    ; ref to bool
   (ref-to-wait-queue reference?)))  ; ref to (listof thread)

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;; used by begin-exp
(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


(define-datatype continuation continuation?
  
  (end-main-thread-cont)           
  (end-subthread-cont)
  
  (diff1-cont                       ; cont[(- [] (value-of e2 env))]
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont                         ; cont[(- val1 [])]
   (val1 expval?)
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont                          ; cont[(apply-proc val1 [])]
   (val1 expval?)
   (cont continuation?))
  (set-rhs-cont
   (loc reference?)
   (cont continuation?))
  
  (spawn-cont 
   (saved-cont continuation?))
  (wait-cont 
   (saved-cont continuation?))
  (signal-cont 
   (saved-cont continuation?))
  
  (unop-arg-cont
   (unop1 unop?)
   (cont continuation?))
  )

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

;;; represent environment as a list of bindings.
;;; binding ::= (id expval) 
;;;           | ((list-of id) (list-of bvar) (list-of expression))

;;; The first binding for extend-env, the second is for
;;; extend-env-rec. 

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

;;; This should probably be factored out into a module called
;;; environments.scm, like it is in most of the other interpreters.

(define empty-env
  (lambda ()
    '()))

(define empty-env? 
  (lambda (x) (null? x)))

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extend-env-rec*
  (lambda (p-names b-vars p-bodies saved-env)
    (cons 
     (list p-names b-vars p-bodies)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env) 
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (if (symbol? (car binding))
              ;; ok, this is an extend-env
              (if (eqv? search-sym (car binding))
                  (cadr binding)
                  (apply-env saved-env search-sym))
              ;; no, this is an extend-env-rec
              (let ((pos (locate search-sym (car binding)))
                    (b-vars (cadr binding))
                    (p-bodies (caddr binding)))
                (if pos
                    (newref 
                     (proc-val
                      (procedure
                       (list-ref b-vars pos)
                       (list-ref p-bodies pos)
                       env)))
                    (apply-env saved-env search-sym))))))))

;; returns position of sym in los, else #f
(define locate
  (lambda (sym los)
    (let loop ((pos 0) (los los))
      ;; los is at position pos of the original los
      (cond
        ((null? los) #f)
        ((eqv? sym (car los)) pos)
        (else (loop (+ pos 1) (cdr los)))))))

(define init-env
  (lambda ()
    (letrec
        ((make-init-env
          ;; entry ::= (id expval)
          (lambda (entries)
            (if (null? entries)
                (empty-env)
                (extend-env 
                 (car (car entries))
                 (newref (cadr (car entries)))
                 (make-init-env (cdr entries)))))))
      (make-init-env
       (list
        (list 'i (num-val 1))
        (list 'v (num-val 5))
        (list 'x (num-val 10)))))))

;; not precise, but will do.
(define environment?
  (list-of
   (lambda (p)
     (and 
      (pair? p)
      (or
       (symbol? (car p))
       ((list-of symbol?) (car p)))))))
