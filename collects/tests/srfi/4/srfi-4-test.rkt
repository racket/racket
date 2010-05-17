(module srfi-4-test mzscheme
  
  (require rackunit)
  (require rackunit/text-ui
           srfi/4)
  
  (provide srfi-4-tests)

  (define-syntax (check-struct-info-binding stx)
    (syntax-case stx ()
        [(_ name)
         (if (identifier? (syntax name))
             #'#t
             #'(fail "Identifier not bound"))]))

  (define-check (check-srfi-4-type make pred length ref set to-list from-list)
    (check-pred pred (from-list '(1 2 3 4)))
    (check-pred list? (to-list (from-list '(1 2 3 4))))
    (check-equal? (to-list (from-list '(1 2 3 4))) '(1 2 3 4))
    (check-pred pred (make 4 1))
    (let ([vec (make 4 1)])
      (check-equal? (ref vec 0) 1)
      (check-equal? (ref vec 1) 1)
      (check-equal? (ref vec 2) 1)
      (check-equal? (ref vec 3) 1)
      (set vec 0 5)
      (check-equal? (ref vec 0) 5)
      (check-equal? (length vec) 4)))

  (define-check (check-srfi-4-float-type make pred length ref set to-list from-list)
    (check-pred pred (from-list '(1. 2. 3. 4.)))
    (check-pred list? (to-list (from-list '(1. 2. 3. 4.))))
    (check-equal? (to-list (from-list '(1. 2. 3. 4.))) '(1. 2. 3. 4.))
    (check-pred pred (make 4 1.))
    (let ([vec (make 4 1.)])
      (check-equal? (ref vec 0) 1.)
      (check-equal? (ref vec 1) 1.)
      (check-equal? (ref vec 2) 1.)
      (check-equal? (ref vec 3) 1.)
      (set vec 0 5.)
      (check-equal? (ref vec 0) 5.)
      (check-equal? (length vec) 4)))
  
  (define srfi-4-tests
    (test-suite
     "All tests for srfi-4"

     (test-case
      "s8"
      (check-srfi-4-type make-s8vector s8vector? s8vector-length s8vector-ref s8vector-set! s8vector->list list->s8vector)
      (check-struct-info-binding s8))

     (test-case
      "u8"
      (check-srfi-4-type make-u8vector u8vector? u8vector-length u8vector-ref u8vector-set! u8vector->list list->u8vector))

     (test-case
      "s16"
      (check-srfi-4-type make-s16vector s16vector? s16vector-length s16vector-ref s16vector-set! s16vector->list list->s16vector)
      (check-struct-info-binding s16))

     (test-case
      "u16"
      (check-srfi-4-type make-u16vector u16vector? u16vector-length u16vector-ref u16vector-set! u16vector->list list->u16vector)
      (check-struct-info-binding u16))

     (test-case
      "s32"
      (check-srfi-4-type make-s32vector s32vector? s32vector-length s32vector-ref s32vector-set! s32vector->list list->s32vector)
      (check-struct-info-binding s32))

     (test-case
      "u32"
      (check-srfi-4-type make-u32vector u32vector? u32vector-length u32vector-ref u32vector-set! u32vector->list list->u32vector)
      (check-struct-info-binding u32))

     (test-case
      "s64"
      (check-srfi-4-type make-s64vector s64vector? s64vector-length s64vector-ref s64vector-set! s64vector->list list->s64vector)
      (check-struct-info-binding s64))

     (test-case
      "u64"
      (check-srfi-4-type make-u64vector u64vector? u64vector-length u64vector-ref u64vector-set! u64vector->list list->u64vector)
      (check-struct-info-binding u64))

     (test-case
      "f32"
      (check-srfi-4-float-type make-f32vector f32vector? f32vector-length f32vector-ref f32vector-set! f32vector->list list->f32vector)
      (check-struct-info-binding f32))

     (test-case
      "f64"
      (check-srfi-4-float-type make-f64vector f64vector? f64vector-length f64vector-ref f64vector-set! f64vector->list list->f64vector)
      (check-struct-info-binding f64))
     
     ))


  (run-tests srfi-4-tests)
  
  )
