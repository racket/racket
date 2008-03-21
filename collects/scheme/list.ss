#lang scheme/base

(provide first second third fourth fifth sixth seventh eighth ninth tenth
         last

         rest

         cons?
         empty
         empty?

         drop
         take

         append*
         flatten)

(define (first x)
  (if (and (pair? x) (list? x))
    (car x)
    (raise-type-error 'first "non-empty list" x)))

(define-syntax define-lgetter
  (syntax-rules ()
    [(_ name npos)
     (define (name l0)
       (if (list? l0)
         (let loop ([l l0] [pos npos])
           (if (pair? l)
             (if (eq? pos 1) (car l) (loop (cdr l) (sub1 pos)))
             (raise-type-error
              'name (format "list with ~a or more items" npos) l0)))
         (raise-type-error 'name "list" l0)))]))
(define-lgetter second  2)
(define-lgetter third   3)
(define-lgetter fourth  4)
(define-lgetter fifth   5)
(define-lgetter sixth   6)
(define-lgetter seventh 7)
(define-lgetter eighth  8)
(define-lgetter ninth   9)
(define-lgetter tenth   10)

(define (last l)
  (if (and (pair? l) (list? l))
    (let loop ([l l])
      (if (pair? (cdr l))
        (loop (cdr l))
        (car l)))
    (raise-type-error 'last "non-empty list" l)))

(define (rest l)
  (if (and (pair? l) (list? l))
    (cdr l)
    (raise-type-error 'rest "non-empty list" l)))

(define cons? (lambda (l) (pair? l)))
(define empty? (lambda (l) (null? l)))
(define empty '())

(define drop list-tail)
(define (take list0 n0)
  (unless (and (integer? n0) (exact? n0))
    (raise-type-error 'take "non-negative integer" n0))
  (let loop ([list list0] [n n0])
    (cond [(zero? n) '()]
          [(pair? list) (cons (car list) (loop (cdr list) (sub1 n)))]
          [else (raise-mismatch-error
                 'take
                 (format "index ~e too large for list~a: ~e"
                         n0
                         (if (list? list) "" " (not a proper list)")
                         list0)
                 n0)])))

(define append*
  (case-lambda [(ls) (apply append ls)] ; optimize common case
               [(ls . lss) (apply append (apply list* ls lss))]))

(define (flatten orig-sexp)
  (let loop ([sexp orig-sexp] [acc null])
    (cond [(null? sexp) acc]
          [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
          [else (cons sexp acc)])))
