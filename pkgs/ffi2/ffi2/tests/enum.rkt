#lang racket/base
(require ffi2
         rackunit)

(let ()
  (define-ffi2-enum shape_t int_t
    circle
    square
    triangle)

  (check-equal? (ffi2-cast 'circle #:from shape_t  #:to int_t) 0)
  (check-equal? (ffi2-cast 'square #:from shape_t  #:to int_t) 1)
  (check-equal? (ffi2-cast 'triangle #:from shape_t  #:to int_t) 2)
  (check-exn exn:fail:contract? (lambda () (ffi2-cast 'oops #:from shape_t  #:to int_t)))

  (check-equal? (ffi2-cast 0 #:to shape_t #:from int_t) 'circle)
  (check-equal? (ffi2-cast 1 #:to shape_t #:from int_t) 'square)
  (check-equal? (ffi2-cast 2 #:to shape_t  #:from int_t) 'triangle)
  (check-equal? (ffi2-cast 3 #:to shape_t  #:from int_t) 3))

(let ()
  (define-ffi2-enum shape_t int_t
    circle
    square = 10
    triangle)

  (check-equal? (ffi2-cast 'circle #:from shape_t  #:to int_t) 0)
  (check-equal? (ffi2-cast 'square #:from shape_t  #:to int_t) 10)
  (check-equal? (ffi2-cast 'triangle #:from shape_t  #:to int_t) 11)
  (check-exn exn:fail:contract? (lambda () (ffi2-cast 'oops #:from shape_t  #:to int_t)))

  (check-equal? (ffi2-cast 0 #:to shape_t #:from int_t) 'circle)
  (check-equal? (ffi2-cast 10 #:to shape_t #:from int_t) 'square)
  (check-equal? (ffi2-cast 11 #:to shape_t  #:from int_t) 'triangle)
  (check-equal? (ffi2-cast 3 #:to shape_t  #:from int_t) 3))

(check-exn (lambda (x)
             (and (exn:fail:syntax? x)
                  (regexp-match? #rx"duplicate symbol" (exn-message x))))
           (lambda ()
             (parameterize ([current-namespace (variable-reference->namespace (#%variable-reference))])
               (eval '(define-ffi2-enum shape_t int_t
                        circle
                        circle)))))
