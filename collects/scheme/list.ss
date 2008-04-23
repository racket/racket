#lang scheme/base

(provide first second third fourth fifth sixth seventh eighth ninth tenth

         last-pair last rest

         cons?
         empty
         empty?

         drop
         take

         append*
         flatten
         add-between
         remove-duplicates
         filter-map
         partition

         ;; convenience
         append-map
         filter-not)

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

(define (last-pair l)
  (if (pair? l)
    (let loop ([l l] [x (cdr l)])
      (if (pair? x)
        (loop x (cdr x))
        l))
    (raise-type-error 'last-pair "pair" l)))

(define (last l)
  (if (and (pair? l) (list? l))
    (let loop ([l l] [x (cdr l)])
      (if (pair? x)
        (loop x (cdr x))
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

(define (flatten orig-sexp)
  (let loop ([sexp orig-sexp] [acc null])
    (cond [(null? sexp) acc]
          [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
          [else (cons sexp acc)])))

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

(define (remove-duplicates l [=? equal?])
  (unless (list? l) (raise-type-error 'remove-duplicates "list" l))
  (let ([h (cond [(< (length l) 40) #f]
                 [(eq? =? eq?) (make-hasheq)]
                 [(eq? =? equal?) (make-hash)]
                 [else #f])])
    (if h
      ;; Using a hash table when the list is long enough and a using `equal?'
      ;; or `eq?'.  The length threshold (40) was determined by trying it out
      ;; with lists of length n holding (random n) numbers.
      (let loop ([l l])
        (if (null? l)
          l
          (let ([x (car l)] [l (cdr l)])
            (if (hash-ref h x #f)
              (loop l)
              (begin (hash-set! h x #t) (cons x (loop l)))))))
      ;; plain n^2 list traversal (optimized for common cases)
      (let-syntax ([loop (syntax-rules ()
                           [(_ search)
                            (let loop ([l l] [seen null])
                              (if (null? l)
                                l
                                (let ([x (car l)] [l (cdr l)])
                                  (if (search x seen)
                                    (loop l seen)
                                    (cons x (loop l (cons x seen)))))))])])
        (cond [(eq? =? equal?) (loop member)]
              [(eq? =? eq?)    (loop memq)]
              [(eq? =? eqv?)   (loop memv)]
              [else (loop (lambda (x seen)
                            (ormap (lambda (y) (=? x y)) seen)))])))))

(define (filter-map f l . ls)
  (unless (and (procedure? f) (procedure-arity-includes? f (add1 (length ls))))
    (raise-type-error
     'filter-map (format "procedure (arity ~a)" (add1 (length ls))) f))
  (unless (and (list? l) (andmap list? ls))
    (raise-type-error
     'filter-map "proper list"
     (ormap (lambda (x) (and (not (list? x)) x)) (cons l ls))))
  (if (pair? ls)
    (let ([len (length l)])
      (if (andmap (lambda (l) (= len (length l))) ls)
        (let loop ([l l] [ls ls])
          (if (null? l)
            null
            (let ([x (apply f (car l) (map car ls))])
              (if x
                (cons x (loop (cdr l) (map cdr ls)))
                (loop (cdr l) (map cdr ls))))))
        (error 'filter-map "all lists must have same size")))
    (let loop ([l l])
      (if (null? l)
        null
        (let ([x (f (car l))])
          (if x (cons x (loop (cdr l))) (loop (cdr l))))))))

;; Originally from srfi-1 -- shares common tail with the input when possible
;; (define (partition f l)
;;   (unless (and (procedure? f) (procedure-arity-includes? f 1))
;;     (raise-type-error 'partition "procedure (arity 1)" f))
;;   (unless (list? l) (raise-type-error 'partition "proper list" l))
;;   (let loop ([l l])
;;     (if (null? l)
;;       (values null null)
;;       (let* ([x (car l)] [x? (f x)])
;;         (let-values ([(in out) (loop (cdr l))])
;;           (if x?
;;             (values (if (pair? out) (cons x in) l) out)
;;             (values in (if (pair? in) (cons x out) l))))))))

;; But that one is slower than this, probably due to value packages
(define (partition pred l)
  (unless (and (procedure? pred) (procedure-arity-includes? pred 1))
    (raise-type-error 'partition "procedure (arity 1)" pred))
  (unless (list? l) (raise-type-error 'partition "proper list" l))
  (let loop ([l l] [i '()] [o '()])
    (if (null? l)
      (values (reverse i) (reverse o))
      (let ([x (car l)] [l (cdr l)])
        (if (pred x) (loop l (cons x i) o) (loop l i (cons x o)))))))

(define append-map
  (case-lambda [(f l) (apply append (map f l))]
               [(f l1 l2) (apply append (map f l1 l2))]
               [(f l . ls) (apply append (apply map f l ls))]))

;; this is an exact copy of `filter' in scheme/private/list, with the
;; `if' branches swapped.
(define (filter-not f list)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 1))
    (raise-type-error 'filter-not "procedure (arity 1)" f))
  (unless (list? list)
    (raise-type-error 'filter-not "proper list" list))
  ;; accumulating the result and reversing it is currently slightly
  ;; faster than a plain loop
  (let loop ([l list] [result null])
    (if (null? l)
      (reverse result)
      (loop (cdr l) (if (f (car l)) result (cons (car l) result))))))
