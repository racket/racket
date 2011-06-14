#lang racket/base

(provide first second third fourth fifth sixth seventh eighth ninth tenth

         last-pair last rest

         cons?
         empty
         empty?

         make-list

         drop
         take
         split-at
         drop-right
         take-right
         split-at-right

         append*
         flatten
         add-between
         remove-duplicates
         filter-map
         count
         partition

         argmin
         argmax

         ;; convenience
         append-map
         filter-not
         shuffle)

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

(define (make-list n x)
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'make-list "non-negative exact integer" n))
  (let loop ([n n] [r '()])
    (if (zero? n) r (loop (sub1 n) (cons x r)))))

;; internal use below
(define (drop* list n) ; no error checking, returns #f if index is too large
  (if (zero? n) list (and (pair? list) (drop* (cdr list) (sub1 n)))))
(define (too-large who list n)
  (raise-mismatch-error
   who
   (format "index ~e too large for list~a: "
           n (if (list? list) "" " (not a proper list)"))
   list))

(define (take list0 n0)
  (unless (exact-nonnegative-integer? n0)
    (raise-type-error 'take "non-negative exact integer" 1 list0 n0))
  (let loop ([list list0] [n n0])
    (cond [(zero? n) '()]
          [(pair? list) (cons (car list) (loop (cdr list) (sub1 n)))]
          [else (too-large 'take list0 n0)])))

(define (split-at list0 n0)
  (unless (exact-nonnegative-integer? n0)
    (raise-type-error 'split-at "non-negative exact integer" 1 list0 n0))
  (let loop ([list list0] [n n0] [pfx '()])
    (cond [(zero? n) (values (reverse pfx) list)]
          [(pair? list) (loop (cdr list) (sub1 n) (cons (car list) pfx))]
          [else (too-large 'split-at list0 n0)])))

(define (drop list n)
  ;; could be defined as `list-tail', but this is better for errors anyway
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'drop "non-negative exact integer" 1 list n))
  (or (drop* list n) (too-large 'drop list n)))

;; take/drop-right are originally from srfi-1, uses the same lead-pointer trick

(define (take-right list n)
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'take-right "non-negative exact integer" 1 list n))
  (let loop ([list list]
             [lead (or (drop* list n) (too-large 'take-right list n))])
    ;; could throw an error for non-lists, but be more like `take'
    (if (pair? lead)
      (loop (cdr list) (cdr lead))
      list)))

(define (drop-right list n)
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'drop-right "non-negative exact integer" n))
  (let loop ([list list]
             [lead (or (drop* list n) (too-large 'drop-right list n))])
    ;; could throw an error for non-lists, but be more like `drop'
    (if (pair? lead)
      (cons (car list) (loop (cdr list) (cdr lead)))
      '())))

(define (split-at-right list n)
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'split-at-right "non-negative exact integer" n))
  (let loop ([list list]
             [lead (or (drop* list n) (too-large 'split-at-right list n))]
             [pfx '()])
    ;; could throw an error for non-lists, but be more like `split-at'
    (if (pair? lead)
      (loop (cdr list) (cdr lead) (cons (car list) pfx))
      (values (reverse pfx) list))))

(define append*
  (case-lambda [(ls) (apply append ls)] ; optimize common case
               [(l1 l2) (apply append l1 l2)]
               [(l1 l2 l3) (apply append l1 l2 l3)]
               [(l1 l2 l3 l4) (apply append l1 l2 l3 l4)]
               [(l . lss) (apply apply append l lss)]))

(define (flatten orig-sexp)
  (let loop ([sexp orig-sexp] [acc null])
    (cond [(null? sexp) acc]
          [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
          [else (cons sexp acc)])))

;; General note: many non-tail recursive, which are just as fast in mzscheme

(define (add-between l x)
  (cond [(not (list? l)) (raise-type-error 'add-between "list" 0 l x)]
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

(define (remove-duplicates l [=? equal?] #:key [key #f])
  ;; `no-key' is used to optimize the case for long lists, it could be done for
  ;; shorter ones too, but that adds a ton of code to the result (about 2k).
  (define-syntax-rule (no-key x) x)
  (unless (list? l) (raise-type-error 'remove-duplicates "list" l))
  (let* ([len (length l)]
         [h (cond [(<= len 1) #t]
                  [(<= len 40) #f]
                  [(eq? =? eq?) (make-hasheq)]
                  [(eq? =? equal?) (make-hash)]
                  [else #f])])
    (case h
      [(#t) l]
      [(#f)
       ;; plain n^2 list traversal (optimized for common cases) for short lists
       ;; and for equalities other than `eq?' or `equal?'  The length threshold
       ;; above (40) was determined by trying it out with lists of length n
       ;; holding (random n) numbers.
       (let ([key (or key (lambda (x) x))])
         (let-syntax ([loop (syntax-rules ()
                              [(_ search)
                               (let loop ([l l] [seen null])
                                 (if (null? l)
                                   l
                                   (let* ([x (car l)] [k (key x)] [l (cdr l)])
                                     (if (search k seen)
                                       (loop l seen)
                                       (cons x (loop l (cons k seen)))))))])])
           (cond [(eq? =? equal?) (loop member)]
                 [(eq? =? eq?)    (loop memq)]
                 [(eq? =? eqv?)   (loop memv)]
                 [else (loop (lambda (x seen)
                               (ormap (lambda (y) (=? x y)) seen)))])))]
      [else
       ;; Use a hash for long lists with simple hash tables.
       (let-syntax ([loop
                     (syntax-rules ()
                       [(_ getkey)
                        (let loop ([l l])
                          (if (null? l)
                            l
                            (let* ([x (car l)] [k (getkey x)] [l (cdr l)])
                              (if (hash-ref h k #f)
                                (loop l)
                                (begin (hash-set! h k #t)
                                       (cons x (loop l)))))))])])
         (if key (loop key) (loop no-key)))])))

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

;; very similar to `filter-map', one more such function will justify some macro
(define (count f l . ls)
  (unless (and (procedure? f) (procedure-arity-includes? f (add1 (length ls))))
    (raise-type-error
     'count (format "procedure (arity ~a)" (add1 (length ls))) f))
  (unless (and (list? l) (andmap list? ls))
    (raise-type-error
     'count "proper list"
     (ormap (lambda (x) (and (not (list? x)) x)) (cons l ls))))
  (if (pair? ls)
    (let ([len (length l)])
      (if (andmap (lambda (l) (= len (length l))) ls)
        (let loop ([l l] [ls ls] [c 0])
          (if (null? l)
            c
            (loop (cdr l) (map cdr ls)
                  (if (apply f (car l) (map car ls)) (add1 c) c))))
        (error 'count "all lists must have same size")))
    (let loop ([l l] [c 0])
      (if (null? l) c (loop (cdr l) (if (f (car l)) (add1 c) c))))))

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

;; But that one is slower than this, probably due to value packaging
(define (partition pred l)
  (unless (and (procedure? pred) (procedure-arity-includes? pred 1))
    (raise-type-error 'partition "procedure (arity 1)" 0 pred l))
  (unless (list? l) (raise-type-error 'partition "proper list" 1 pred l))
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
    (raise-type-error 'filter-not "procedure (arity 1)" 0 f list))
  (unless (list? list)
    (raise-type-error 'filter-not "proper list" 1 f list))
  ;; accumulating the result and reversing it is currently slightly
  ;; faster than a plain loop
  (let loop ([l list] [result null])
    (if (null? l)
      (reverse result)
      (loop (cdr l) (if (f (car l)) result (cons (car l) result))))))

(define (shuffle l)
  (sort l < #:key (lambda (_) (random)) #:cache-keys? #t))

;; mk-min : (number number -> boolean) symbol (X -> real) (listof X) -> X
(define (mk-min cmp name f xs)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 1))
    (raise-type-error name "procedure (arity 1)" 0 f xs))
  (unless (and (list? xs)
               (pair? xs))
    (raise-type-error name "non-empty list" 1 f xs))
  (let ([init-min-var (f (car xs))])
    (unless (real? init-min-var)
      (raise-type-error name "procedure that returns real numbers" 0 f xs))
    (let loop ([min (car xs)]
               [min-var init-min-var]
               [xs (cdr xs)])
      (cond
        [(null? xs) min]
        [else
         (let ([new-min (f (car xs))])
           (unless (real? new-min)
             (raise-type-error name "procedure that returns real numbers" 0 f xs))
           (cond
             [(cmp new-min min-var)
              (loop (car xs) new-min (cdr xs))]
             [else
              (loop min min-var (cdr xs))]))]))))

(define (argmin f xs) (mk-min < 'argmin f xs))
(define (argmax f xs) (mk-min > 'argmax f xs))
