
(module list scheme/base

  (provide first
           second
           third
           fourth
           fifth
           sixth
           seventh
           eighth

           rest

           cons?
           empty
           empty?)

  (define (first x)
    (unless (and (pair? x) 
                 (list? x))
      (raise-type-error 'first "non-empty list" x))
    (car x))

  (define-syntax define-lgetter
    (syntax-rules ()
      [(_ name npos)
       (define (name l0)
         (if (list? l0)
             (let loop ([l l0] [pos npos])
               (if (pair? l)
                   (if (eq? pos 1) (car l) (loop (cdr l) (sub1 pos)))
                   (raise-type-error 'name (format "list with ~a or more items" npos) l0)))
             (raise-type-error 'name "list" l0)))]))
  (define-lgetter second  2)
  (define-lgetter third   3)
  (define-lgetter fourth  4)
  (define-lgetter fifth   5)
  (define-lgetter sixth   6)
  (define-lgetter seventh 7)
  (define-lgetter eighth  8)

  (define (rest x)
    (unless (and (pair? x)
                 (list? x))
      (raise-type-error 'rest "non-empty list" x))
    (cdr x))

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
