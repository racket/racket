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
         add-between
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
               [(l . lss) (apply append (apply list* l lss))]))

;; General note: many non-tail recursive, which are just as fast in mzscheme

(define (add-between l x)
  (cond [(not (list? l)) (raise-type-error 'add-between "list" l)]
        [(null? l) null]
        [(null? (cdr l)) l]
        [else (cons (car l)
                    (let loop ([l (cdr l)])
                      (if (null? l)
                        null
                        (list* x (car l) (loop (cdr l))))))]))

;; This is nice for symmetry, but confusing to use, and we can get it using
;; something like (append* (add-between l ls)), or even `flatten' for an
;; arbitrary nesting.
;; (define (lists-join ls l)
;;   (cond [(null? ls) ls]
;;         [(null? l) ls] ; empty separator
;;         [else (append (car ls)
;;                       (let loop ([ls (cdr ls)])
;;                         (if (null? ls)
;;                           ls
;;                           (append l (car ls) (loop (cdr ls))))))]))

(define (flatten orig-sexp)
  (let loop ([sexp orig-sexp] [acc null])
    (cond [(null? sexp) acc]
          [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
          [else (cons sexp acc)])))
