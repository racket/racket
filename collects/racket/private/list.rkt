
(module list "pre-base.rkt"

  (provide split-at
           
           foldl
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
  
  (define (split-at list0 n0)
    (unless (exact-nonnegative-integer? n0)
      (raise-type-error 'split-at "non-negative exact integer" n0))
    (let loop ([list list0] [n n0] [pfx '()])
      (cond [(zero? n) (values (reverse pfx) list)]
            [(pair? list) (loop (cdr list) (sub1 n) (cons (car list) pfx))]
            [else 
             (raise-mismatch-error
              'split-at
              (format "index ~e too large for list~a: "
                      n0 (if (list? list0) "" " (not a proper list)"))
              list0)])))

  (#%require (rename "sort.rkt" raw-sort sort)
             (for-syntax "stxcase-scheme.rkt"))

  (provide sort)
  (define (sort lst less? #:key [getkey #f] #:cache-keys? [cache-keys? #f])
    (unless (list? lst) (raise-type-error 'sort "proper list" lst))
    (unless (and (procedure? less?) (procedure-arity-includes? less? 2))
      (raise-type-error 'sort "procedure of arity 2" less?))
    (when (and getkey (not (and (procedure? getkey)
                                (procedure-arity-includes? getkey 1))))
      (raise-type-error 'sort "procedure of arity 1" getkey))
    ;; don't provide the extra args if not needed, it's a bit faster
    (if getkey (raw-sort lst less? getkey cache-keys?) (raw-sort lst less?)))

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

  (define (check-fold name proc init l more)
    (unless (procedure? proc)
      (apply raise-type-error name "procedure" 0 proc init l more))
    (unless (list? l)
      (apply raise-type-error name "list" 2 proc init l more))
    (if (null? more)
        (unless (procedure-arity-includes? proc 2)
          (raise-mismatch-error name "arity mismatch, does not accept 1 argument: " proc))
        (let ([len (length l)])
          (let loop ([more more][n 3])
            (unless (null? more)
              (unless (list? (car more))
                (apply raise-type-error name "list" n proc init l more))
              (unless (= len (length (car more)))
                (raise-mismatch-error name 
                                      "given list does not have the same size as the first list: "
                                      (car more)))
              (loop (cdr more) (add1 n))))
          (unless (procedure-arity-includes? proc (+ 2 (length more)))
            (raise-mismatch-error name 
                                  (format "arity mismatch, does not accept ~a arguments: " 
                                          (add1 (length more)))
                                  proc)))))

  (define foldl
    (case-lambda
      [(f init l)
       (check-fold 'foldl f init l null)
       (let loop ([init init] [l l])
         (if (null? l) init (loop (f (car l) init) (cdr l))))]
      [(f init l . ls)
       (check-fold 'foldl f init l ls)
       (let loop ([init init] [ls (cons l ls)])
         (cond [(andmap pair? ls)
                (loop (apply f (mapadd car ls init)) (map cdr ls))]
               [(ormap pair? ls)
                (error 'foldl "received non-equal length input lists")]
               [else init]))]))

  (define foldr
    (case-lambda
      [(f init l)
       (check-fold 'foldr f init l null)
       (let loop ([init init] [l l])
         (if (null? l)
           init
           (f (car l) (loop init (cdr l)))))]
      [(f init l . ls)
       (check-fold 'foldr f init l ls)
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
    ;; accumulating the result and reversing it is currently slightly
    ;; faster than a plain loop
    (let loop ([l list] [result null])
      (if (null? l)
        (reverse result)
        (loop (cdr l) (if (f (car l)) (cons (car l) result) result)))))

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
    (let recr ([j 0] [i n])
      (cond [(zero? i) null]
            [else (cons (fcn j)
                        (recr (add1 j) (sub1 i)))])))

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
                   (call-with-values (lambda () (g a)) f))
                 (lambda args
                   (call-with-values (lambda () (apply g args)) f)))))]
      [() values]
      [(f . more)
       (if (procedure? f)
           (let ([m (apply compose more)])
             (compose f m))
           (compose f))])))
