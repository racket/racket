
(module list mzscheme

  (provide set-first!
           first
           second
           third
           fourth
           fifth
           sixth
           seventh
           eighth

           set-rest!
           rest

           cons?
           empty
           empty?

           foldl
           foldr

           last-pair

           remv
           remq
           remove
           remv*
           remq*
           remove*

           assf
           memf
           findf

           filter

           quicksort ; deprecated
           mergesort ; deprecated
           sort
           sort!
           merge-sorted-lists
           merge-sorted-lists!)

  ;; used by sort-internal, but can be useful by itself
  (define (merge-sorted-lists! a b less?)
    (define (loop r a b r-a?) ; r-a? for optimization -- is r connected to a?
      (if (less? (car b) (car a))
        (begin (when r-a? (set-cdr! r b))
               (if (null? (cdr b)) (set-cdr! b a) (loop b a (cdr b) #f)))
        ;; (car a) <= (car b)
        (begin (unless r-a? (set-cdr! r a))
               (if (null? (cdr a)) (set-cdr! a b) (loop a (cdr a) b #t)))))
    (cond [(null? a) b]
          [(null? b) a]
          [(less? (car b) (car a))
           (if (null? (cdr b)) (set-cdr! b a) (loop b a (cdr b) #f))
           b]
          [else ; (car a) <= (car b)
           (if (null? (cdr a)) (set-cdr! a b) (loop a (cdr a) b #t))
           a]))

  ;; a non-destructive version for symmetry with merge-sorted-lists!
  (define (merge-sorted-lists a b less?)
    (cond [(null? a) b]
          [(null? b) a]
          [else (let loop ([x (car a)] [a (cdr a)] [y (car b)] [b (cdr b)])
                  ;; The loop handles the merging of non-empty lists.  It has
                  ;; been written this way to save testing and car/cdring.
                  (if (less? y x)
                    (if (null? b)
                      (list* y x a)
                      (cons y (loop x a (car b) (cdr b))))
                    ;; x <= y
                    (if (null? a)
                      (list* x y b)
                      (cons x (loop (car a) (cdr a) y b)))))]))

  ;; This is a destructive stable merge-sort, adapted from slib and improved by
  ;; Eli Barzilay
  ;; The original source said:
  ;;   It uses a version of merge-sort invented, to the best of my knowledge,
  ;;   by David H. D. Warren, and first used in the DEC-10 Prolog system.
  ;;   R. A. O'Keefe adapted it to work destructively in Scheme.
  ;; but it's a plain destructive merge sort.
  (define (sort-internal lst less? copy? who)
    (define (step n)
      (cond [(> n 3) (let* (; let* not really needed with mzscheme's l->r eval
                            [j (quotient n 2)] [a (step j)] [b (step (- n j))])
                       (merge-sorted-lists! a b less?))]
            ;; the following two cases are just explicit treatment of sublists
            ;; of length 2 and 3, could remove both (and use the above case for
            ;; n>1) and it would still work, except a little slower
            [(= n 3) (let ([p lst] [p1 (cdr lst)] [p2 (cddr lst)])
                       (let ([x (car p)] [y (car p1)] [z (car p2)])
                         (set! lst (cdr p2))
                         (cond [(less? y x) ; y x
                                (cond [(less? z y) ; z y x
                                       (set-car! p  z)
                                       (set-car! p1 y)
                                       (set-car! p2 x)]
                                      [(less? z x) ; y z x
                                       (set-car! p        y)
                                       (set-car! p1 z)
                                       (set-car! p2 x)]
                                      [else ; y x z
                                       (set-car! p  y)
                                       (set-car! p1 x)])]
                               [(less? z x) ; z x y
                                (set-car! p  z)
                                (set-car! p1 x)
                                (set-car! p2 y)]
                               [(less? z y) ; x z y
                                (set-car! p1 z)
                                (set-car! p2 y)])
                         (set-cdr! p2 '())
                         p))]
            [(= n 2) (let ([x (car lst)] [y (cadr lst)] [p lst])
                       (set! lst (cddr lst))
                       (when (less? y x) (set-car! p y) (set-car! (cdr p) x))
                       (set-cdr! (cdr p) '())
                       p)]
            [(= n 1) (let ([p lst])
                       (set! lst (cdr lst))
                       (set-cdr! p '())
                       p)]
            [else '()]))
    (unless (list? lst)
      (raise-type-error who "proper list" lst))
    (unless (and (procedure? less?) (procedure-arity-includes? less? 2))
      (raise-type-error who "procedure of arity 2" less?))
    (let ([n (length lst)])
      (cond [(<= n 1) lst]
            ;; check if the list is already sorted
            ;; (which can be a common case, eg, directory lists).
            [(let loop ([last (car lst)] [next (cdr lst)])
               (or (null? next)
                   (and (not (less? (car next) last))
                        (loop (car next) (cdr next)))))
             lst]
            [else (when copy? (set! lst (append lst '())))
                  (step n)])))

  (define (sort! lst less?) (sort-internal lst less? #f 'sort!))
  (define (sort  lst less?) (sort-internal lst less? #t 'sort))

  ;; deprecated!
  (define quicksort sort)
  (define mergesort sort)

  (define remove
    (letrec ([rm (case-lambda 
                  [(item list) (rm item list equal?)]
                  [(item list equal?)
                   (let loop ([list list])
                     (cond [(null? list) ()]
                           [(equal? item (car list)) (cdr list)]
                           [else (cons (car list) (loop (cdr list)))]))])])
      rm))

  (define (remq item list)
    (remove item list eq?))

  (define (remv item list)
    (remove item list eqv?))

  (define remove*
    (case-lambda
      [(l r equal?)
       (cond
	[(null? r) null]
	[else (let ([first-r (car r)])
		(let loop ([l-rest l])
		  (cond
		   [(null? l-rest) (cons first-r (remove* l (cdr r) equal?))]
		   [(equal? (car l-rest) first-r) (remove* l (cdr r) equal?)]
		   [else (loop (cdr l-rest))])))])]
      [(l r) (remove* l r equal?)]))

  (define (remq* l r)
    (remove* l r eq?))

  (define (remv* l r)
    (remove* l r eqv?))

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

  (define-syntax (define-finder stx)
    (syntax-case stx ()
      [(_ name mode)
       (and (identifier? #'name) (memq (syntax-e #'mode) '(assoc member find)))
       #`(define (name f list)
           (unless (and (procedure? f) (procedure-arity-includes? f 1))
             (raise-type-error 'name "procedure (arity 1)" f))
           (let loop ([l list])
             (cond
              [(null? l) #f]
              [(not (pair? l))
               (raise
                (make-exn:fail:contract
                 (format "~a: second argument must be a (proper) list; given ~e"
                         'name list)
                 (current-continuation-marks)))]
              [else (let ([a (car l)])
                      #,(case (syntax-e #'mode)
                          [(member) #'(if (f a) l (loop (cdr l)))]
                          [(find)   #'(if (f a) a (loop (cdr l)))]
                          [(assoc)  #'(if (pair? a)
                                        (if (f (car a)) a (loop (cdr l)))
                                        (raise-mismatch-error
                                         'name "found a non-pair in the list: "
                                         a))]))])))]))
  (define-finder assf  assoc)
  (define-finder memf  member)
  (define-finder findf find)

  (define (filter f list)
    (unless (and (procedure? f)
                 (procedure-arity-includes? f 1))
      (raise-type-error 'filter "procedure (arity 1)" f))
    ;; We use the reverse! trick because it's too easy to
    ;;  overflow the internal stack using natural recursion.
    ;; It's too bad that our Scheme system is so bad, but
    ;;  until someone fixes it...
    (let loop ([l list] [result null])
      (cond
       [(null? l) (reverse! result)]
       [(pair? l) (loop (cdr l) (if (f (car l)) (cons (car l) result) result))]
       [else (raise-mismatch-error
              'filter "expects a proper list: " list)])))

  (define (first x)
    (unless (pair? x) (raise-type-error 'first "non-empty list" x))
    (car x))
  (define (set-first! x v)
    (unless (pair? x) (raise-type-error 'set-first! "non-empty list" x))
    (set-car! x v))
  (define-syntax define-lgetter
    (syntax-rules ()
      [(_ name npos)
       (define (name l0)
         (let loop ([l l0] [pos npos])
           (if (pair? l)
             (if (eq? pos 1) (car l) (loop (cdr l) (sub1 pos)))
             (raise-type-error
              'name (format "list with ~a or more items" npos) l0))))]))
  (define-lgetter second  2)
  (define-lgetter third   3)
  (define-lgetter fourth  4)
  (define-lgetter fifth   5)
  (define-lgetter sixth   6)
  (define-lgetter seventh 7)
  (define-lgetter eighth  8)

  (define (rest x)
    (unless (pair? x)
      (raise-type-error 'rest "non-empty list" x))
    (cdr x))

  (define (set-rest! x v)
    (unless (pair? x)
      (raise-type-error 'set-rest! "non-empty list" x))
    (unless (or (null? v) (pair? v))
      (raise-type-error 'set-rest! "second argument must be a list" v))
    (set-cdr! x v))

  (define (last-pair l)
    (if (pair? l)
      (let loop ([l l] [x (cdr l)])
        (if (pair? x)
          (loop x (cdr x))
          l))
      (raise-type-error 'last-pair "pair" l)))

  (define cons? (lambda (x) (pair? x)))
  (define empty? (lambda (x) (null? x)))
  (define empty '()))

