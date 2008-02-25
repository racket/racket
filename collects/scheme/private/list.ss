
(module list "pre-base.ss"

  (provide foldl
           foldr

           remv
           remq
           remove
           remv*
           remq*
           remove*

           memf
           assf
           findf

           filter

           sort

           build-vector
           build-string
           build-list

           compose)

  (#%require (for-syntax "stxcase-scheme.ss"))

  ;; This is a destructive stable merge-sort, adapted from slib and improved by
  ;; Eli Barzilay
  ;; The original source said:
  ;;   It uses a version of merge-sort invented, to the best of my knowledge,
  ;;   by David H. D. Warren, and first used in the DEC-10 Prolog system.
  ;;   R. A. O'Keefe adapted it to work destructively in Scheme.
  ;; but it's a plain destructive merge sort, which I optimized further.
  (define sort-internal
    (let ()
      (define-syntax sort-internal-body
        (syntax-rules ()
          [(_ lst less? n)
           (begin
             (define (merge-sorted! a b)
               ;; r-a? for optimization -- is r connected to a?
               (define (loop r a b r-a?)
                 (if (less? (mcar b) (mcar a))
                     (begin
                       (when r-a? (set-mcdr! r b))
                       (if (null? (mcdr b)) (set-mcdr! b a) (loop b a (mcdr b) #f)))
                     ;; (car a) <= (car b)
                     (begin
                       (unless r-a? (set-mcdr! r a))
                       (if (null? (mcdr a)) (set-mcdr! a b) (loop a (mcdr a) b #t)))))
               (cond [(null? a) b]
                     [(null? b) a]
                     [(less? (mcar b) (mcar a))
                      (if (null? (mcdr b)) (set-mcdr! b a) (loop b a (mcdr b) #f))
                      b]
                     [else ; (car a) <= (car b)
                      (if (null? (mcdr a)) (set-mcdr! a b) (loop a (mcdr a) b #t))
                      a]))
             (let step ([n n])
               (cond [(> n 3)
                      (let* (; let* not really needed with mzscheme's l->r eval
                             [j (quotient n 2)] [a (step j)] [b (step (- n j))])
                        (merge-sorted! a b))]
                     ;; the following two cases are just explicit treatment of
                     ;; sublists of length 2 and 3, could remove both (and use the
                     ;; above case for n>1) and it would still work, except a
                     ;; little slower
                     [(= n 3) (let ([p lst] [p1 (mcdr lst)] [p2 (mcdr (mcdr lst))])
                                (let ([x (mcar p)] [y (mcar p1)] [z (mcar p2)])
                                  (set! lst (mcdr p2))
                                  (cond [(less? y x) ; y x
                                         (cond [(less? z y) ; z y x
                                                (set-mcar! p  z)
                                                (set-mcar! p1 y)
                                                (set-mcar! p2 x)]
                                               [(less? z x) ; y z x
                                                (set-mcar! p        y)
                                                (set-mcar! p1 z)
                                                (set-mcar! p2 x)]
                                               [else ; y x z
                                                (set-mcar! p  y)
                                                (set-mcar! p1 x)])]
                                        [(less? z x) ; z x y
                                         (set-mcar! p  z)
                                         (set-mcar! p1 x)
                                         (set-mcar! p2 y)]
                                        [(less? z y) ; x z y
                                         (set-mcar! p1 z)
                                         (set-mcar! p2 y)])
                                  (set-mcdr! p2 '())
                                  p))]
                     [(= n 2) (let ([x (mcar lst)] [y (mcar (mcdr lst))] [p lst])
                                (set! lst (mcdr (mcdr lst)))
                                (when (less? y x)
                                  (set-mcar! p y)
                                  (set-mcar! (mcdr p) x))
                                (set-mcdr! (mcdr p) '())
                                p)]
                     [(= n 1) (let ([p lst])
                                (set! lst (mcdr lst))
                                (set-mcdr! p '())
                                p)]
                     [else '()])))]))
      (define sort-internals (make-hash-table))
      (define-syntax make-precompiled-sort
        (syntax-rules ()
          [(_ less?) (hash-table-put! sort-internals less?
                                      (lambda (lst n) (sort-internal-body lst less? n)))]))
      (define ((sort-internal* less?) lst n)
        (sort-internal-body lst less? n))
      (make-precompiled-sort <)
      (make-precompiled-sort string<?)
      (make-precompiled-sort string-ci<?)
      (make-precompiled-sort keyword<?)
      (lambda (less? lst n)
        ((or (hash-table-get sort-internals less? #f)
             (sort-internal* less?))
         lst n))))

  (define (sort lst less?)
    (unless (list? lst)
      (raise-type-error 'sort "proper list" lst))
    (unless (and (procedure? less?) (procedure-arity-includes? less? 2))
      (raise-type-error 'sort "procedure of arity 2" less?))
    (let ([n (length lst)])
      (cond
        ;; trivial case
        [(< n 2) lst]
        ;; check if the list is already sorted
        ;; (which can be a common case, eg, directory lists).
        [(let loop ([last (car lst)] [next (cdr lst)])
           (or (null? next)
               (and (not (less? (car next) last))
                    (loop (car next) (cdr next)))))
         lst]
        ;; inlined cases, for optimization of short lists
        [(< n 3)
         (if (= n 2)
           ;; (because of the above test, we can assume that the input is
           ;; unsorted)
           (list (cadr lst) (car lst))
           (let ([a (car lst)] [b (cadr lst)] [c (caddr lst)])
             ;; General note: we need a stable sort, so we should always
             ;; compare (less? later-item earlier-item) since it gives more
             ;; information.  A good way to see that we have good code is to
             ;; check that each permutation appears exactly once.  This means
             ;; that n=4 will have 23 cases, so don't bother.  (Homework: write
             ;; a macro to generate code for a specific N.  Bonus: prove
             ;; correctness.  Extra bonus: prove optimal solution.  Extra extra
             ;; bonus: prove optimal solution exists, extract macro from
             ;; proof.)
             (let ([a (car lst)] [b (cadr lst)] [c (caddr lst)])
               (if (less? b a)
                 ;; b<a
                 (if (less? c b)
                   (list c b a)
                   ;; b<a, b<=c
                   (if (less? c a) (list b c a) (list b a c)))
                 ;; a<=b, so c<b (b<=c is impossible due to above test)
                 (if (less? c a) (list c a b) (list a c b))))))]
        [else (let (;; list->mlist
                    [mlst (let ([mlst (mcons (car lst) null)])
                            (let loop ([last mlst] [lst (cdr lst)])
                              (if (null? lst)
                                mlst
                                (let ([new (mcons (car lst) null)])
                                  (set-mcdr! last new)
                                  (loop new (cdr lst))))))])
                ;; mlist->list
                (let loop ([r (sort-internal less? mlst n)])
                  (if (null? r)
                    r
                    (cons (mcar r) (loop (mcdr r))))))])))

  (define (do-remove who item list equal?)
    (unless (list? list)
      (raise-type-error who "list" list))
    (let loop ([list list])
      (cond [(null? list) null]
            [(equal? item (car list)) (cdr list)]
            [else (cons (car list) (loop (cdr list)))])))

  (define remove
    (case-lambda
      [(item list) (do-remove 'remove item list equal?)]
      [(item list equal?)
       (unless (and (procedure? equal?)
                    (procedure-arity-includes? equal? 2))
         (raise-type-error 'remove "procedure (arity 2)" equal?))
       (do-remove 'remove item list equal?)]))

  (define (remq item list)
    (do-remove 'remq item list eq?))

  (define (remv item list)
    (do-remove 'remv item list eqv?))

  (define (do-remove* who l r equal?)
    (unless (list? l)
      (raise-type-error who "list" l))
    (unless (list? r)
      (raise-type-error who "list" r))
    (let rloop ([r r])
      (cond
        [(null? r) null]
        [else (let ([first-r (car r)])
                (let loop ([l-rest l])
                  (cond
                    [(null? l-rest) (cons first-r (rloop (cdr r)))]
                    [(equal? (car l-rest) first-r) (rloop (cdr r))]
                    [else (loop (cdr l-rest))])))])))

  (define remove*
    (case-lambda
      [(l r) (do-remove* 'remove* l r equal?)]
      [(l r equal?)
       (unless (and (procedure? equal?)
                    (procedure-arity-includes? equal? 2))
         (raise-type-error 'remove* "procedure (arity 2)" equal?))
       (do-remove* 'remove* l r equal?)]))

  (define (remq* l r)
    (do-remove* 'remq* l r eq?))

  (define (remv* l r)
    (do-remove* 'remv* l r eqv?))

  (define (memf f list)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (raise-type-error 'memf "procedure (arity 1)" f))
    (let loop ([l list])
      (cond
       [(null? l) #f]
       [(not (pair? l))
        (raise-mismatch-error 'memf
                              "not a proper list: "
                              list)]
       [else (if (f (car l)) l (loop (cdr l)))])))

  (define (findf f list)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (raise-type-error 'findf "procedure (arity 1)" f))
    (let loop ([l list])
      (cond
       [(null? l) #f]
       [(not (pair? l))
        (raise-mismatch-error 'findf
                              "not a proper list: "
                              list)]
       [else (let ([a (car l)])
               (if (f a)
                   a
                   (loop (cdr l))))])))

  (define (assf f list)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (raise-type-error 'assf "procedure (arity 1)" f))
    (let loop ([l list])
      (cond
       [(null? l) #f]
       [(not (pair? l))
        (raise-mismatch-error 'assf
                              "not a proper list: "
                              list)]
       [else (let ([a (car l)])
               (if (pair? a)
                   (if (f (car a)) 
                       a
                       (loop (cdr l)))
                   (raise-mismatch-error 'assf
                                         "found a non-pair in the list: "
                                         a)))])))

  ;; fold : ((A B -> B) B (listof A) -> B)
  ;; fold : ((A1 ... An B -> B) B (listof A1) ... (listof An) -> B)

  ;; foldl builds "B" from the beginning of the list to the end of the
  ;; list and foldr builds the "B" from the end of the list to the
  ;; beginning of the list.

  (define (mapadd f l last)
    (let loop ([l l])
      (if (null? l)
        (list last)
        (cons (f (car l)) (loop (cdr l))))))

  (define foldl
    (case-lambda
      [(f init l)
       (let loop ([init init] [l l])
         (if (null? l) init (loop (f (car l) init) (cdr l))))]
      [(f init l . ls)
       (let loop ([init init] [ls (cons l ls)])
         (cond [(andmap pair? ls)
                (loop (apply f (mapadd car ls init)) (map cdr ls))]
               [(ormap pair? ls)
                (error 'foldl "received non-equal length input lists")]
               [else init]))]))

  (define foldr
    (case-lambda
      [(f init l)
       (let loop ([init init] [l l])
         (if (null? l)
           init
           (f (car l) (loop init (cdr l)))))]
      [(f init l . ls)
       (let loop ([ls (cons l ls)])
         (cond [(andmap pair? ls)
                (apply f (mapadd car ls (loop (map cdr ls))))]
               [(ormap pair? ls)
                (error 'foldr "received non-equal length input lists")]
               [else init]))]))

  (define (filter f list)
    (unless (and (procedure? f)
                 (procedure-arity-includes? f 1))
      (raise-type-error 'filter "procedure (arity 1)" f))
    (unless (list? list)
      (raise-type-error 'filter "proper list" list))
    ;; We use `reverse' because it's easy to
    ;;  overflow the internal stack using natural recursion.
    ;;  It's not clear that it matters, though...
    (let loop ([l list] [result null])
      (cond
       [(null? l) (reverse result)]
       [else (loop (cdr l) (if (f (car l)) (cons (car l) result) result))])))

  ;; (build-vector n f) returns a vector 0..n-1 where the ith element is (f i).
  ;; The eval order is guaranteed to be: 0, 1, 2, ..., n-1.
  ;; eg: (build-vector 4 (lambda (i) i)) ==> #4(0 1 2 3)
  (define (build-vector n fcn)
    (unless (exact-nonnegative-integer? n)
      (raise-type-error 'build-vector "exact-nonnegative-integer" n))
    (unless (and (procedure? fcn)
                 (procedure-arity-includes? fcn 1))
      (raise-type-error 'build-vector "procedure (arity 1)" fcn))
    (let ([vec (make-vector n)])
      (let loop ((i 0))
        (if (= i n)
          vec
          (begin (vector-set! vec i (fcn i)) (loop (add1 i)))))))

  (define (build-string n fcn)
    (unless (exact-nonnegative-integer? n)
      (raise-type-error 'build-string "exact-nonnegative-integer" n))
    (unless (and (procedure? fcn)
                 (procedure-arity-includes? fcn 1))
      (raise-type-error 'build-string "procedure (arity 1)" fcn))
    (let ([str (make-string n)])
      (let loop ((i 0))
        (if (= i n)
            str
            (begin (string-set! str i (fcn i)) (loop (add1 i)))))))
  
  (define (build-list n fcn)
    (unless (exact-nonnegative-integer? n)
      (raise-type-error 'build-list "exact-nonnegative-integer" n))
    (unless (and (procedure? fcn)
                 (procedure-arity-includes? fcn 1))
      (raise-type-error 'build-list "procedure (arity 1)" fcn))
    (if (zero? n)
      '()
      (let loop ([i 0] [a null])
        (if (= i n)
            (reverse a)
            (loop (add1 i)
                  (cons (fcn i) a))))))

  (define compose
    (case-lambda
      [(f) (if (procedure? f) 
               f
               (raise-type-error 'compose "procedure" f))]
      [(f g)
       (let ([f (compose f)]
             [g (compose g)])
         (if (eqv? 1 (procedure-arity f)) ; optimize: don't use call-w-values
             (if (eqv? 1 (procedure-arity g)) ; optimize: single arity everywhere
                 (lambda (x) (f (g x)))
                 (lambda args (f (apply g args))))
             (if (eqv? 1 (procedure-arity g)) ; optimize: single input
                 (lambda (a)
                   (call-with-values
                    (lambda () (g a))
                    f))
                 (lambda args
                   (call-with-values
                    (lambda () (apply g args))
                    f)))))]
      [(f . more)
       (if (procedure? f)
           (let ([m (apply compose more)])
             (compose f m))
           (compose f))])))
