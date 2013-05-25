#lang racket
(require (for-syntax syntax/struct))

(define-syntax (exp stx)
  (syntax-case stx ()
    [(_ sel? set? (name sup) (field ...))
     (with-syntax ([e (build-struct-generation #'name
                                               (syntax->list #'(field ...))
                                               (not (syntax-e #'sel?))
                                               (not (syntax-e #'set?))
                                               (and (syntax-e #'sup) #'sup))]
                   [(id ...)
                    (build-struct-names #'name
                                        (syntax->list #'(field ...))
                                        (not (syntax-e #'sel?))
                                        (not (syntax-e #'set?)))])
       #'(define-values (id ...) e))]))

(define (check a b)
  (unless (equal? a b) (error "failed!")))

(let ([set-pt-x! 12])
  (exp #t #f (pt #f) (x y))
  (check 10 (pt-x (make-pt 10 20)))
  (check 20 (pt-y (make-pt 10 20)))
  (check 12 set-pt-x!))

(let ([set-pt-x! 12])
  (exp #t #t (pt #f) (x y))
  (check 10 (pt-x (make-pt 10 20)))
  (check 20 (pt-y (make-pt 10 20)))
  (check #t (procedure? set-pt-x!)))
