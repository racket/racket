
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
      (test '((11 21 31 40.25) 500.25) quint->list* (list*->quint '((11 21 31 40.25) 500.25)))
      
      (define-cstruct _due [(x _byte #:offset 0) (y _byte #:offset 1)])
      (define cv (make-due 255 1))
      (test 255 due-x cv)
      (test 1 due-y cv)
      (test '(255 1) due->list cv)
      (set-due-x! cv 1)
      (test 1 due-x cv)
      (test 1 due-y cv)
      (define-cstruct _due2 [(x _byte #:offset 0) (y _byte)])
      (test 1 due2-y (cast cv _due-pointer _due2-pointer))
      
      (define-cstruct (_tre _due) [(z _short #:offset 2)])
      (define dv (make-tre 255 1 20))
      (test 255 due-x dv)
      (test 1 due-y dv)
      (test 20 tre-z dv)
      (set-due-y! dv 255)
      (test 255 due-y dv)
      (test 20 tre-z dv)
      
      (define-cstruct _quattro [(pre _tre) (v _int #:offset 4)])
      (define qtv (make-quattro dv 50))
      (test 255 due-x qtv)
      (test 255 due-y qtv)
      (test 20 tre-z qtv)
      (test 50 quattro-v qtv)
      (set-tre-z! qtv 255)
      (test 255 tre-z qtv)
      (test '((255 255 255) 50) quattro->list* qtv)
      (test '((255 127 1) 2048) quattro->list* (list*->quattro '((255 127 1) 2048)))))

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
(syntax-test #'(define-cstruct _y ([x _int]) #:alignment) #rx"missing expression for #:alignment")
(syntax-test #'(define-cstruct _y ([x _int]) #:alignment 2 #:alignment 2) #rx"multiple specifications of #:alignment")
(syntax-test #'(define-cstruct _y ([x _int]) #:malloc-mode) #rx"missing expression for #:malloc-mode")
(syntax-test #'(define-cstruct _y ([x _int]) #:malloc-mode 'atomic #:malloc-mode 'atomic) #rx"multiple specifications of #:malloc-mode")
(syntax-test #'(define-cstruct _y ([x _int]) #:property) #rx"missing property expression for #:property")
(syntax-test #'(define-cstruct _y ([x _int]) #:property x)  #rx"missing value expression for #:property")
(syntax-test #'(define-cstruct _y ([x _int]) #:property x y . 10))
(syntax-test #'(define-cstruct _y ([x _int]) #:no-equal #:no-equal) #rx"multiple specifications of #:no-equal")

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
;; Check to ensure offsets are computed correctly

(let ()
  (test '(0 4 8) compute-offsets (list _int _bool _string))
  (test '(0 4 5 8) compute-offsets (list _int _byte _byte _int))
  (test '(0 4 5 6) compute-offsets (list _int _byte _byte _int) 1)
  (test '(5 4 3 2) compute-offsets (list _int _byte _byte _int) #f (list 5 4 3 2))
  (test '(0 5 6 8) compute-offsets (list _int _byte _byte _int) #f (list #f 5 #f #f)))

;; ----------------------------------------

(report-errs)
