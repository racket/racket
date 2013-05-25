
(load-relative "loadtest.rktl")

(Section 'cstruct)

(require ffi/unsafe)

(let ()
  (define-syntax-rule (make-test [t-prop ...] t-more
                                 [q-prop ...] q-more)
    (begin
      (define-cstruct _tri ([x _int] [y _short] [z _short])
        t-prop ...)
      (define tv (make-tri 1 2 3))
      (define tv2 (list->tri '(4 5 6)))
      (test 1 tri-x tv)
      (test 2 tri-y tv)
      (test 3 tri-z tv)
      (test 6 tri-z tv2)
      (test '(4 5 6) tri->list tv2)
      (test '(4 5 6) tri->list* tv2)
      (test '(7 8 9) tri->list* (list*->tri '(7 8 9)))
      (test #f cast #f _tri-pointer/null _pointer)
      (test #f cast #f _pointer _tri-pointer/null)
      (test #t tri? tv)
      (test #f tri? 'tv)
      (test #f tri? (cast 1 _intptr _pointer))
      (t-more tv)
      (t-more tv2)

      (define tv3 (cast tv _tri-pointer _tri-pointer))
      (t-more tv3)

      (define-cstruct (_quad _tri) ([q _double])
        q-prop ...)
      (define qv (make-quad 10 20 30 40.0))
      (test 10 tri-x qv)
      (test 20 tri-y qv)
      (test 30 tri-z qv)
      (test 40.0 quad-q qv)
      (test #t tri? tv)
      (test #f tri? 'tv)
      (test #f tri? (cast 1 _intptr _pointer))
      (q-more qv)
      (test #f cast #f _quad-pointer/null _pointer)
      (test #f cast #f _pointer _quad-pointer/null)

      (define-cstruct _quint ([pre _quad]
                              [r _float]))
      (define kv (make-quint qv 500.0))
      (test 10 tri-x kv)
      (test 20 tri-y kv)
      (test 30 tri-z kv)
      (test 40.0 quad-q kv)
      (test 500.0 quint-r kv)
      (test 500.0 cadr (quint->list kv))
      (test '((10 20 30 40.0) 500.0) quint->list* kv)
      (test '((11 21 31 40.25) 500.25) quint->list* (list*->quint '((11 21 31 40.25) 500.25)))))

  (make-test [] void
             [] void)
  (make-test [#:property prop:procedure (lambda (self) self)] 
             (lambda (tv)
               (test tv tv))
             [#:property prop:evt always-evt]
             (lambda (qv)
               (test always-evt sync qv))))

(syntax-test #'(define-cstruct))
(syntax-test #'(define-cstruct _x))
(syntax-test #'(define-cstruct #f))
(syntax-test #'(define-cstruct x ()))
(syntax-test #'(define-cstruct #f ()))
(syntax-test #'(define-cstruct _y (y)))
(syntax-test #'(define-cstruct _y () #:alignment))
(syntax-test #'(define-cstruct _y () #:alignment 2 #:alignment 2))
(syntax-test #'(define-cstruct _y () #:property))
(syntax-test #'(define-cstruct _y () #:property x))
(syntax-test #'(define-cstruct _y () #:property x y . 10))
(syntax-test #'(define-cstruct _y () #:no-equal #:no-equal))

;; ----------------------------------------
;; Check struct properties and subtypes:

(let ()
  (define-values (p p? p-ref) (make-struct-type-property 'my-p))
  (define-cstruct _S ([a (_array _byte 23)])
    #:property p (lambda () _S))
  (define s (ptr-ref (malloc _S) _S)) ; dummy instance
  struct:cpointer:S
  (test #t p? struct:cpointer:S)
  (test #t p? s)

  (define-cstruct (_Q _S) ())
  (test #t p? (ptr-ref (malloc _Q) _Q))

  (define-cstruct _Z ([a (_array _byte 23)]))
  (define-cstruct (_W _Z) ()
    #:property p (lambda () _Z))
  (test #f p? (ptr-ref (malloc _Z) _Z))
  (test #t p? (ptr-ref (malloc _W) _W))
  (test #t p? struct:cpointer:W)
  (test #t Z? (ptr-ref (malloc _W) _W)))

;; ----------------------------------------
;; Check struct properties and equality:

(let ()
  (define-cstruct _B ([a _int]) 
    #:property prop:procedure void)
  
  (define b (make-B 123))
  
  (test #t equal? b (cast b _B-pointer _B-pointer))) ; cast forces new wrapper

(let ()
  (define-cstruct _B ([a _int]) 
    #:property prop:procedure void
    #:no-equal)
  
  (define b (make-B 123))
  
  (test #f equal? b (cast b _B-pointer _B-pointer))) ; cast forces new wrapper

;; ----------------------------------------

(report-errs)
