(module list frtime/frtime-lang-only
  
  (require 
   (lifted (only-in racket/base sort) sort)
   
   (lifted (only-in racket/list fifth) 
           fifth)
   (lifted (only-in racket/list sixth) sixth)
   (lifted (only-in racket/list seventh) seventh)
   (lifted (only-in racket/list eighth) eighth)
   (lifted (only-in racket/list last-pair) last-pair)
  
           (rename-in (except-in racket/list first rest second third fourth empty? cons? fifth sixth seventh eighth last-pair) [empty empty]))

  (define first car)
  (define rest cdr)
  (define second cadr)
  (define third caddr)
  (define fourth cadddr)
  
  (define empty? null?)
  
  (define remove
    (letrec ([rm (case-lambda
                   [(item list) (rm item list equal?)]
                   [(item list equal?)
                    (let loop ([list list])
                      (cond
                        [(null? list) '()]
                        [(equal? item (car list)) (cdr list)]
                        [else (cons (car list)
                                    (loop (cdr list)))]))])])
      rm))
  
  (define remq
    (lambda (item list)
      (remove item list eq?)))
  
  (define remv
    (lambda (item list)
      (remove item list eqv?)))
  
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
  
  (define remq*
    (lambda (l r)
      (remove* l r eq?)))
  
  (define remv*
    (lambda (l r)
      (remove* l r eqv?)))
  
  (define mapadd
    (lambda (f l last)
      (letrec ((helper
                (lambda (l)
                  (cond
                    [(null? l) (list last)]
                    [else (cons (f (car l)) (helper (cdr l)))]))))
        (helper l))))
  
  (define foldl
    (letrec ((fold-one
              (lambda (f init l)
                (letrec ((helper
                          (lambda (init l)
                            (cond
                              [(null? l) init]
                              [else (helper (f (car l) init) (cdr l))]))))
                  (helper init l))))
             (fold-n
              (lambda (f init  l)
                (cond
                  [(ormap null? l)
                   (if (andmap null? l) 
                       init
                       (error 'foldl "received non-equal length input lists"))]
                  [else (fold-n
                         f
                         (apply f (mapadd car l init))
                         (map cdr l))]))))
      (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))])))
  
  (define foldr
    (letrec ((fold-one
              (lambda (f init l)
                (letrec ((helper
                          (lambda (init l)
                            (cond
                              [(null? l) init]
                              [else (f (car l) (helper init (cdr l)))]))))
                  (helper init l))))
             (fold-n
              (lambda (f init l)
                (cond
                  [(ormap null? l)
                   (if (andmap null? l)
                       init
                       (error 'foldr "received non-equal length input lists"))]
                  [else (apply f
                               (mapadd car l
                                       (fold-n f init (map cdr l))))]))))
      (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))])))
  
  (define make-find
    (lambda (name whole-list?)
       (lambda (f list)
         (unless (and (procedure? f)
                      (procedure-arity-includes? f 1))
           (raise-type-error name "procedure (arity 1)" f))
         (let loop ([l list])
           (cond
             [(null? l) #f]
             [(not (pair? l)) 
              (raise (make-exn:fail
                      (format "~a: second argument must be a (proper) list; given ~e" name list)
                      (current-continuation-marks)))]
             [else (let ([a (car l)])
                     (if whole-list?
                         (if (f a)
                             l
                             (loop (cdr l)))
                         (if (pair? a)
                             (if (f (car a))
                                 a
                                 (loop (cdr l)))
                             (raise-mismatch-error
                              name
                              "found a non-pair in the list: "
                              a))))])))))

  (define assf
    (let ([a (make-find 'assf #f)])
       (lambda (f l)
         (a f l))))
  
  (define memf
    (let ([a (make-find 'memf #t)])
       (lambda (f l)
         (a f l))))

  
  (define (filter f l)
    (list-match
     l
     (lambda (a d) (if (f a)
                       (cons a (filter f d))
                       (filter f d)))
     (lambda () empty)))
;      [(empty? l) empty]
;      [(f (first l)) (cons (first l) (filter f (rest l)))]
;      [else (filter f (rest l))]))
  
  
  (define (cons? x) (pair? x))
  
  (provide (all-defined-out) empty))
