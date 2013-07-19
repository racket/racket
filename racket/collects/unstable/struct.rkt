#lang racket/base
;; owner: ryanc
(require (for-syntax racket/base
                     racket/struct-info))
(provide make
         struct->list
         (for-syntax get-struct-info))

;; get-struct-info : identifier stx -> struct-info-list
(define-for-syntax (get-struct-info id ctx)
  (define (bad-struct-name x)
    (raise-syntax-error #f "expected struct name" ctx x))
  (unless (identifier? id)
    (bad-struct-name id))
  (let ([value (syntax-local-value id (lambda () #f))])
    (unless (struct-info? value)
      (bad-struct-name id))
    (extract-struct-info value)))

;; (make struct-name field-expr ...)
;; Checks that correct number of fields given.
(define-syntax (make stx)
  (syntax-case stx ()
    [(make S expr ...)
     (let ()
       (define info (get-struct-info #'S stx))
       (define constructor (list-ref info 1))
       (define accessors (list-ref info 3))
       (unless (identifier? #'constructor)
         (raise-syntax-error #f "constructor not available for struct" stx #'S))
       (unless (andmap identifier? accessors)
         (raise-syntax-error #f "incomplete info for struct type" stx #'S))
       (let ([num-slots (length accessors)]
             [num-provided (length (syntax->list #'(expr ...)))])
         (unless (= num-provided num-slots)
           (raise-syntax-error
            #f
            (format "wrong number of arguments for struct ~s (expected ~s, got ~s)"
                    (syntax-e #'S)
                    num-slots
                    num-provided)
            stx)))
       (with-syntax ([constructor constructor])
         (syntax-property #'(constructor expr ...)
                          'disappeared-use
                          #'S)))]))
;; Eli: You give a good point for this, but I'd prefer if the optimizer would
;;   detect these, so you'd get the same warnings for constructors too when you
;;   use `-W warning'.  (And then, if you really want these things to be
;;   errors, then perhaps something at the racket level should make it throw
;;   errors instead of warnings.)

(define dummy-value (box 'dummy))

;; struct->list : struct?
;;                #:on-opaque (or/c 'error 'return-false 'skip)
;;             -> (listof any/c)
(define (struct->list s
                      #:on-opaque [on-opaque 'error])
  (define error-on-opaque? (eq? on-opaque 'error))
  (let ([vec (struct->vector s dummy-value)])
    ;; go through vector backwards, don't traverse 0 (struct name)
    (let loop ([index (sub1 (vector-length vec))]
               [elems null]
               [any-opaque? #f])
      (cond [(positive? index)
             (let ([elem (vector-ref vec index)])
               (cond [(eq? elem dummy-value)
                      (when error-on-opaque?
                        (raise-type-error 'struct->list "non-opaque struct" s))
                      (loop (sub1 index) elems #t)]
                     [else (loop (sub1 index) (cons elem elems) any-opaque?)]))]
            [else
             (cond [(and any-opaque? (eq? on-opaque 'return-false))
                    #f]
                   [else elems])]))))
