(module list "pre-base.rkt"
  (require (rename-in "reverse.rkt" [alt-reverse reverse]))

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

           assq
           assv
           assoc

           filter

           sort

           build-vector
           build-string
           build-list

           reverse

           compose
           compose1)

  (#%require (rename "sort.rkt" raw-sort sort)
             (for-syntax "stxcase-scheme.rkt")
             (only '#%unsafe unsafe-car unsafe-cdr))

  (provide sort)
  (define (sort lst less? #:key [getkey #f] #:cache-keys? [cache-keys? #f])
    (unless (list? lst) (raise-argument-error 'sort "list?" lst))
    (unless (and (procedure? less?) (procedure-arity-includes? less? 2))
      (raise-argument-error 'sort "(any/c any/c . -> . any/c)" less?))
    (when (and getkey (not (and (procedure? getkey)
                                (procedure-arity-includes? getkey 1))))
      (raise-argument-error 'sort "(any/c . -> . any/c)" getkey))
    ;; don't provide the extra args if not needed, it's a bit faster
    (if getkey (raw-sort lst less? getkey cache-keys?) (raw-sort lst less?)))

  (define (do-remove who item list equal?)
    (unless (list? list)
      (raise-argument-error who "list?" list))
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
         (raise-argument-error 'remove "(any/c any/c . -> . any/c)" equal?))
       (do-remove 'remove item list equal?)]))

  (define (remq item list)
    (do-remove 'remq item list eq?))

  (define (remv item list)
    (do-remove 'remv item list eqv?))

  (define (do-remove* who l r equal?)
    (unless (list? l)
      (raise-argument-error who "list?" l))
    (unless (list? r)
      (raise-argument-error who "list?" r))
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
         (raise-argument-error 'remove* "(any/c any/c . -> . any/c)" equal?))
       (do-remove* 'remove* l r equal?)]))

  (define (remq* l r)
    (do-remove* 'remq* l r eq?))

  (define (remv* l r)
    (do-remove* 'remv* l r eqv?))

  (define (memf f list)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (raise-argument-error 'memf "(any/c . -> any/c)" f))
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
      (raise-argument-error 'findf "(any/c . -> . any/c)" f))
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

  (define (bad-list who orig-l)
    (raise-mismatch-error who
                          "not a proper list: "
                          orig-l))
  (define (bad-item who a orig-l)
    (raise-mismatch-error who
                          "non-pair found in list: "
                          a
                          " in "
                          orig-l))

  (define-values (assq assv assoc assf)
    (let ()
      (define-syntax-rule (assoc-loop who x orig-l is-equal?)
        (let loop ([l orig-l][t orig-l])
          (cond
           [(pair? l)
            (let ([a (unsafe-car l)])
              (if (pair? a)
                  (if (is-equal? x (unsafe-car a))
                      a
                      (let ([l (unsafe-cdr l)])
                        (cond
                         ;; [(eq? l t) (bad-list who orig-l)]
                         [(pair? l)
                          (let ([a (unsafe-car l)])
                            (if (pair? a)
                                (if (is-equal? x (unsafe-car a))
                                    a
                                    (let ([t (unsafe-cdr t)]
                                          [l (unsafe-cdr l)])
                                      (if (eq? l t) 
                                          (bad-list who orig-l)
                                          (loop l t))))
                                (bad-item who a orig-l)))]
                         [(null? l) #f]
                         [else (bad-list who orig-l)])))
                  (bad-item who a orig-l)))]
           [(null? l) #f]
           [else (bad-list who orig-l)])))
      (let ([assq
             (lambda (x l)
               (assoc-loop 'assq x l eq?))]
            [assv
             (lambda (x l)
               (assoc-loop 'assv x l eqv?))]
            [assoc
             (case-lambda
               [(x l) (assoc-loop 'assoc x l equal?)]
               [(x l is-equal?)
                (unless (and (procedure? is-equal?)
                             (procedure-arity-includes? is-equal? 2))
                  (raise-argument-error 'assoc "(any/c any/c . -> . any/c)" is-equal?))
                (assoc-loop 'assoc x l is-equal?)])]
            [assf
             (lambda (f l)
               (unless (and (procedure? f) (procedure-arity-includes? f 1))
                 (raise-argument-error 'assf "(any/c any/c . -> . any/c)" f))
               (assoc-loop 'assf #f l (lambda (_ a) (f a))))])
        (values assq assv assoc assf))))

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
      (apply raise-argument-error name "procedure?" 0 proc init l more))
    (unless (list? l)
      (apply raise-argument-error name "list?" 2 proc init l more))
    (if (null? more)
        (unless (procedure-arity-includes? proc 2)
          (raise-mismatch-error name "given procedure does not accept 2 arguments: " proc))
        (let ([len (length l)])
          (let loop ([more more][n 3])
            (unless (null? more)
              (unless (list? (car more))
                (apply raise-argument-error name "list?" n proc init l more))
              (unless (= len (length (car more)))
                (raise-mismatch-error name 
                                      "given list does not have the same size as the first list: "
                                      (car more)))
              (loop (cdr more) (add1 n))))
          (unless (procedure-arity-includes? proc (+ 2 (length more)))
            (raise-mismatch-error name 
                                  (format "given procedure does not accept ~a arguments: " 
                                          (+ 2 (length more)))
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
         (if (pair? (car ls)) ; `check-fold' ensures all lists have equal length
             (loop (apply f (mapadd car ls init)) (map cdr ls))
             init))]))

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
         (if (pair? (car ls)) ; `check-fold' ensures all lists have equal length
             (apply f (mapadd car ls (loop (map cdr ls))))
             init))]))

  (define (filter f list)
    (unless (and (procedure? f)
                 (procedure-arity-includes? f 1))
      (raise-argument-error 'filter "(any/c . -> . any/c)" f))
    (unless (list? list)
      (raise-argument-error 'filter "list?" list))
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
      (raise-argument-error 'build-vector "exact-nonnegative-integer?" n))
    (unless (and (procedure? fcn)
                 (procedure-arity-includes? fcn 1))
      (raise-argument-error 'build-vector "(exact-nonnegative-integer? . -> . any/c)" fcn))
    (let ([vec (make-vector n)])
      (let loop ((i 0))
        (if (= i n)
          vec
          (begin (vector-set! vec i (fcn i)) (loop (add1 i)))))))

  (define (build-string n fcn)
    (unless (exact-nonnegative-integer? n)
      (raise-argument-error 'build-string "exact-nonnegative-integer?" n))
    (unless (and (procedure? fcn)
                 (procedure-arity-includes? fcn 1))
      (raise-argument-error 'build-string "(exact-nonnegative-integer? . -> . char?)" fcn))
    (let ([str (make-string n)])
      (let loop ((i 0))
        (if (= i n)
            str
            (begin (string-set! str i (fcn i)) (loop (add1 i)))))))

  (define (build-list n fcn)
    (unless (exact-nonnegative-integer? n)
      (raise-argument-error 'build-list "exact-nonnegative-integer?" n))
    (unless (and (procedure? fcn)
                 (procedure-arity-includes? fcn 1))
      (raise-argument-error 'build-list "(exact-nonnegative-integer? . -> . any/c)" fcn))
    (let recr ([j 0] [i n])
      (cond [(zero? i) null]
            [else (cons (fcn j)
                        (recr (add1 j) (sub1 i)))])))

  (define-values [compose1 compose]
    (let ()
      (define-syntax-rule (app1 E1 E2) (E1 E2))
      (define-syntax-rule (app* E1 E2) (call-with-values (lambda () E2) E1))
      (define-syntax-rule (mk-simple-compose app f g)
        (let*-values
            ([(arity) (procedure-arity g)]
             [(required-kwds allowed-kwds) (procedure-keywords g)]
             [(composed)
              ;; FIXME: would be nice to use `procedure-rename',
              ;; `procedure-reduce-arity' and `procedure-reduce-keyword-arity'
              ;; in the places marked below, but they currently add a
              ;; significant overhead.
              (if (eq? 1 arity)
                (lambda (x) (app f (g x)))
                (case-lambda          ; <--- here
                  [(x)   (app f (g x))]
                  [(x y) (app f (g x y))]
                  [args  (app f (apply g args))]))])
          (if (null? allowed-kwds)
            composed
            (make-keyword-procedure   ; <--- and here
             (lambda (kws kw-args . xs)
               (app f (keyword-apply g kws kw-args xs)))
             composed))))
      (define-syntax-rule (can-compose* name n g f fs)
        (unless (null? (let-values ([(req _) (procedure-keywords g)]) req))
          (apply raise-argument-error 'name "procedure-with-no-required-keywords?"
                 n f fs)))
      (define-syntax-rule (can-compose1 name n g f fs)
        (begin (unless (procedure-arity-includes? g 1)
                 (apply raise-argument-error 'name "(any/c . -> . any/c)" n f fs))
               ;; need to check this too (see PR 11978)
               (can-compose* name n g f fs)))
      (define (pipeline1 f rfuns)
        ;; (very) slightly slower alternative:
        ;; (if (null? rfuns)
        ;;   f
        ;;   (pipeline1 (let ([fst (car rfuns)]) (lambda (x) (fst (f x))))
        ;;              (cdr rfuns)))
        (lambda (x)
          (let loop ([x x] [f f] [rfuns rfuns])
            (if (null? rfuns)
              (f x)
              (loop (f x) (car rfuns) (cdr rfuns))))))
      (define (pipeline* f rfuns)
        ;; use the other composition style in this case, to optimize an
        ;; occasional arity-1 procedure in the pipeline
        (if (eqv? 1 (procedure-arity f))
          ;; if `f' is single arity, then going in reverse they will *all* be
          ;; single arities
          (let loop ([f f] [rfuns rfuns])
            (if (null? rfuns)
              f
              (loop (let ([fst (car rfuns)])
                      (if (eqv? 1 (procedure-arity fst))
                        (lambda (x) (fst (f x)))
                        (lambda (x) (app* fst (f x)))))
                    (cdr rfuns))))
          ;; otherwise, going in reverse means that they're all n-ary, which
          ;; means that the list of arguments will be built for each stage, so
          ;; to avoid that go forward in this case
          (let ([funs (reverse (cons f rfuns))])
            (let loop ([f (car funs)] [funs (cdr funs)])
              (if (null? funs)
                f
                (loop (let ([fst (car funs)])
                        (if (eqv? 1 (procedure-arity f))
                          (if (eqv? 1 (procedure-arity fst))
                            (lambda (x) (f (fst x)))
                            (lambda xs (f (apply fst xs))))
                          (if (eqv? 1 (procedure-arity fst))
                            (lambda (x) (app* f (fst x)))
                            (lambda xs (app* f (apply fst xs))))))
                      (cdr funs)))))))
      (define-syntax-rule (mk name app can-compose pipeline mk-simple-compose)
        (define name
          (let ([simple-compose mk-simple-compose])
            (case-lambda
              [(f)
               (if (procedure? f) f (raise-argument-error 'name "procedure?" 0 f))]
              [(f g)
               (unless (procedure? f)
                 (raise-argument-error 'name "procedure?" 0 f g))
               (unless (procedure? g)
                 (raise-argument-error 'name "procedure?" 1 f g))
               (can-compose name 0 f f '())
               (simple-compose f g)]
              [() values]
              [(f0 . fs0)
               (let loop ([f f0] [fs fs0] [i 0] [rfuns '()])
                 (unless (procedure? f)
                   (apply raise-argument-error 'name "procedure?" i f0 fs0))
                 (if (pair? fs)
                   (begin (can-compose name i f f0 fs0)
                          (loop (car fs) (cdr fs) (add1 i) (cons f rfuns)))
                   (simple-compose (pipeline (car rfuns) (cdr rfuns)) f)))]))))
      (mk compose1 app1 can-compose1 pipeline1
          (lambda (f g) (mk-simple-compose app1 f g)))
      (mk compose  app* can-compose* pipeline*
          (lambda (f g)
            (if (eqv? 1 (procedure-arity f))
              (mk-simple-compose app1 f g)
              (mk-simple-compose app* f g))))
      (values compose1 compose)))
  )
