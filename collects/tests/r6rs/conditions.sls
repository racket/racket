#!r6rs

(library (tests r6rs conditions)
  (export run-conditions-tests)
  (import (rnrs)
          (tests r6rs test))


  (define-syntax test-cond
    (syntax-rules ()
      [(_ &c &parent (make arg ...) pred sel ...)
       (begin
         (test (pred (make arg ...)) #t)
         (let ([v (make arg ...)])
           (test (sel v) arg) ...
           'ok)
         (test ((record-predicate (record-type-descriptor &parent)) (make arg ...)) #t)
         (test (record-type-parent (record-type-descriptor &c)) (record-type-descriptor &parent)))]))

  ;; ----------------------------------------

  (define-record-type (&cond1 make-cond1 real-cond1?)
    (parent &condition)
    (fields
     (immutable x real-cond1-x)))
  
  (define cond1?
    (condition-predicate
     (record-type-descriptor &cond1)))
  (define cond1-x
    (condition-accessor
     (record-type-descriptor &cond1)
     real-cond1-x))
  
  (define foo (make-cond1 'foo))
  
  (define-record-type (&cond2 make-cond2 real-cond2?)
    (parent &condition)
    (fields
     (immutable y real-cond2-y)))

  (define cond2?
    (condition-predicate
     (record-type-descriptor &cond2)))
  (define cond2-y
    (condition-accessor
     (record-type-descriptor &cond2)
     real-cond2-y))

  (define bar (make-cond2 'bar))

  (define-condition-type &c &condition
    make-c c?
    (x c-x))

  (define-condition-type &c1 &c
    make-c1 c1?
    (a c1-a))

  (define-condition-type &c2 &c
    make-c2 c2?
    (b c2-b))

  (define v1 (make-c1 "V1" "a1"))

  (define v2 (make-c2 "V2" "b2"))

  (define v3 (condition
              (make-c1 "V3/1" "a3")
              (make-c2 "V3/2" "b3")))

  (define v4 (condition v1 v2))

  (define v5 (condition v2 v3))

  ;; ----------------------------------------

  (define (run-conditions-tests)

    (test (condition? foo) #t)
    (test (cond1? foo) #t)
    (test (cond1-x foo) 'foo)

    (test (condition? (condition foo bar)) #t)
    (test (cond1? (condition foo bar)) #t)
    (test (cond2? (condition foo bar)) #t)
    (test (cond1? (condition foo)) #t)
    (test/unspec (real-cond1? (condition foo)))
    (test (real-cond1? (condition foo bar)) #f)
    (test (cond1-x (condition foo bar)) 'foo)
    (test (cond2-y (condition foo bar)) 'bar)
                
    (test (simple-conditions (condition foo bar))
          (list foo bar))

    (test (simple-conditions
           (condition foo (condition bar)))
          (list foo bar))
    
    (test (c? v1)        #t)
    (test (c1? v1)       #t)
    (test (c2? v1)       #f)
    (test (c-x v1)       "V1")
    (test (c1-a v1)      "a1")

    (test (c? v2)        #t)
    (test (c1? v2)       #f)
    (test (c2? v2)       #t)
    (test (c-x v2)       "V2")
    (test (c2-b v2)      "b2")

    (test (c? v3)        #t)
    (test (c1? v3)       #t)
    (test (c2? v3)       #t)
    (test (c-x v3)       "V3/1")
    (test (c1-a v3)      "a3")
    (test (c2-b v3)      "b3")

    (test (c? v4)        #t)
    (test (c1? v4)       #t)
    (test (c2? v4)       #t)
    (test (c-x v4)       "V1")
    (test (c1-a v4)      "a1")
    (test (c2-b v4)      "b2")

    (test (c? v5)        #t)
    (test (c1? v5)       #t)
    (test (c2? v5)       #t)
    (test (c-x v5)       "V2")
    (test (c1-a v5)      "a3")
    (test (c2-b v5)      "b2")

    (test-cond &message &condition
               (make-message-condition "message")
               message-condition?
               condition-message)
    
    (test-cond &warning &condition
               (make-warning)
               warning?)
    
    (test-cond &serious &condition
               (make-serious-condition)
               serious-condition?)

    (test-cond &error &serious
               (make-error)
               error?)

    (test-cond &violation &serious
               (make-violation)
               violation?)

    (test-cond &assertion &violation
               (make-assertion-violation)
               assertion-violation?)

    (test-cond &irritants &condition
               (make-irritants-condition (list 'sand 'salt 'acid))
               irritants-condition?
               condition-irritants)

    (test-cond &who &condition
               (make-who-condition 'new-boss)
               who-condition?
               condition-who)

    (test-cond &non-continuable &violation
               (make-non-continuable-violation)
               non-continuable-violation?)

    (test-cond &implementation-restriction &violation
              (make-implementation-restriction-violation)
              implementation-restriction-violation?)

    (test-cond &lexical &violation
               (make-lexical-violation)
               lexical-violation?)
    
    (test-cond &syntax &violation
               (make-syntax-violation '(lambda (x) case) 'case)
               syntax-violation?
               syntax-violation-form
               syntax-violation-subform)

    (test-cond &undefined &violation
               (make-undefined-violation)
               undefined-violation?)

    ;; These tests really belong in io/ports.ss:

    (test-cond &i/o &error
               (make-i/o-error)
               i/o-error?)

    (test-cond &i/o-read &i/o
               (make-i/o-read-error)
               i/o-read-error?)

    (test-cond &i/o-write &i/o
               (make-i/o-write-error)
               i/o-write-error?)


    (test-cond &i/o-invalid-position &i/o
               (make-i/o-invalid-position-error 10)
               i/o-invalid-position-error?
               i/o-error-position)

    (test-cond &i/o-filename &i/o
               (make-i/o-filename-error "bad.txt")
               i/o-filename-error?
               i/o-error-filename)

    (test-cond &i/o-file-protection &i/o-filename
               (make-i/o-file-protection-error "private.txt")
               i/o-file-protection-error?
               i/o-error-filename)

    (test-cond &i/o-file-is-read-only  &i/o-file-protection
               (make-i/o-file-is-read-only-error "const.txt")
               i/o-file-is-read-only-error?
               i/o-error-filename)

    (test-cond &i/o-file-already-exists &i/o-filename
               (make-i/o-file-already-exists-error "x.txt")
               i/o-file-already-exists-error?
               i/o-error-filename)

    (test-cond &i/o-file-does-not-exist &i/o-filename
               (make-i/o-file-does-not-exist-error "unicorn.txt")
               i/o-file-does-not-exist-error?
               i/o-error-filename)

    (test-cond &i/o-port &i/o
               (make-i/o-port-error "Hong Kong")
               i/o-port-error?
               i/o-error-port)

    (test-cond &i/o-decoding &i/o-port
               (make-i/o-decoding-error "Hong Kong")
               i/o-decoding-error?
               i/o-error-port)

    (test-cond &i/o-encoding &i/o-port
               (make-i/o-encoding-error "Hong Kong" #\$)
               i/o-encoding-error?
               i/o-error-port
               i/o-encoding-error-char)
    
    ;;
    ))

