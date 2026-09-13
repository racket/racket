;; included by "procs.rktl"

(define-syntax (apply-to-all-procs stx)
  (syntax-case stx ()
    [(_ form data ...)
     #`(form data ...
             #,@(map (lambda (sym)
                       (datum->syntax #'form sym))
                     '(f0
                       f0+
                       f0+/drop1
                       f1
                       f1-m
                       f1+
                       f1+/drop1
                       f0:a
                       f0:a?
                       f1:a
                       f1:a?
                       f1+:a
                       f1+:a?
                       f1+:a/drop
                       f1+:a?/drop
                       f2+:a?/drop
                       f0:a:b
                       f0:a?:b
                       f1:a:b
                       f1:a?:b
                       f1+:a:b
                       f1+:a?:b
                       f0:a:b?
                       f0:a?:b?
                       f1:a:b?
                       f1:a?:b?
                       f1+:a:b?
                       f1+:a?:b?
                       f1+2:a:b
                       f_
                       f_1_2
                       f_0_2+
                       f1:+
                       f1:+/drop)))]))

(define (f0) null)
(define (f0+ . x) x)
(define (f0+/drop1 . x) (cdr x))
(define (f1 x) (list x))
(define f1-m
  (let-syntax ([m (lambda (stx)
                    (syntax-property #'(lambda (x) (list x))
                                     'method-arity-error
                                     #t))])
    m))
(define (f1+ x . rest) (cons x rest))
(define (f1+/drop1 x . rest) rest)
(define (f0:a #:a a) (list a))
(define (f0:a? #:a [a 0]) (list a))
(define (f1:a x #:a a) (list x a))
(define (f1:a? x #:a [a 0]) (list x a))
(define (f1+:a x #:a a . args) (list* x a args))
(define (f1+:a? x #:a [a 0] . args) (list* x a args))
(define (f1+:a/drop x #:a a . args) (if (null? args)
                                        (list a)
                                        (list* (car args) a (cdr args))))
(define (f1+:a?/drop x #:a [a 0] . args) (if (null? args)
                                             (list a)
                                             (list* (car args) a (cdr args))))
(define (f2+:a?/drop x y #:a [a 0] . args) (list* y a args))
(define (f0:a:b #:a a #:b b) (list a b))
(define (f0:a?:b #:a [a 0] #:b b) (list a b))
(define (f1:a:b x #:a a #:b b) (list x a b))
(define (f1:a?:b x #:a [a 0] #:b b) (list x a b))
(define (f1+:a:b x #:a a #:b b . args) (list* x a b args))
(define (f1+:a?:b x #:a [a 0] #:b b . args) (list* x a b args))
(define (f0:a:b? #:a a #:b [b 1]) (list a b))
(define (f0:a?:b? #:a [a 0] #:b [b 1]) (list a b))
(define (f1:a:b? x #:a a #:b [b 1]) (list x a b))
(define (f1:a?:b? x #:a [a 0] #:b [b 1]) (list x a b))
(define (f1+:a:b? x #:a a #:b [b 1] . args) (list* x a b args))
(define (f1+:a?:b? x #:a [a 0] #:b [b 1] . args) (list* x a b args))
(define (f1+2:a:b x [y #f] #:a a #:b b) (if y
                                            (if (number? x)
                                                (list x y a b)
                                                (list y a b))
                                            (list x a b)))
(define f_ (case-lambda))
(define f_1_2 (case-lambda
               [(x) (list x)]
               [(x y) (list x y)]))
(define f_0_2+ (case-lambda
               [() null]
               [(x y . args) (list* x y args)]))
(define f1:+ (make-keyword-procedure
              (lambda (kws kw-args x)
                (cons x kw-args))
              (let ([f1:+ (lambda (x) (list x))])
                f1:+)))
(define f1:+/drop (make-keyword-procedure
                   (lambda (kws kw-args x)
                     kw-args)
                   (lambda (x) null)))
