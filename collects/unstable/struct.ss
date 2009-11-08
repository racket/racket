#lang scheme/base
;; owner: ryanc
(require (for-syntax scheme/base
                     scheme/struct-info))
(provide make)

;; (make struct-name field-expr ...)
;; Checks that correct number of fields given.
(define-syntax (make stx)
  (define (bad-struct-name x)
    (raise-syntax-error #f "expected struct name" stx x))
  (define (get-struct-info id)
    (unless (identifier? id)
      (bad-struct-name id))
    (let ([value (syntax-local-value id (lambda () #f))])
      (unless (struct-info? value)
        (bad-struct-name id))
      (extract-struct-info value)))
  (syntax-case stx ()
    [(make S expr ...)
     (let ()
       (define info (get-struct-info #'S))
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
            (format "wrong number of arguments for struct ~s (expected ~s)"
                    (syntax-e #'S)
                    num-slots)
            stx)))
       (with-syntax ([constructor constructor])
         (syntax-property #'(constructor expr ...)
                          'disappeared-use
                          #'S)))]))
