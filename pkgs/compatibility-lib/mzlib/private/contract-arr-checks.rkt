#lang racket/base

(provide (all-defined-out))
(require racket/contract/private/blame
         racket/contract/private/misc)

(define empty-case-lambda/c
  (flat-named-contract '(case->)
                       (λ (x) (and (procedure? x) (null? (procedure-arity x))))))

;; ----------------------------------------
;; Checks and error functions used in macro expansions

;; procedure-accepts-and-more? : procedure number -> boolean
;; returns #t if val accepts dom-length arguments and
;; any number of arguments more than dom-length. 
;; returns #f otherwise.
(define (procedure-accepts-and-more? val dom-length)
  (let ([arity (procedure-arity val)])
    (cond
      [(number? arity) #f]
      [(arity-at-least? arity)
       (<= (arity-at-least-value arity) dom-length)]
      [else
       (let ([min-at-least (let loop ([ars arity]
                                      [acc #f])
                             (cond
                               [(null? ars) acc]
                               [else (let ([ar (car ars)])
                                       (cond
                                         [(arity-at-least? ar)
                                          (if (and acc
                                                   (< acc (arity-at-least-value ar)))
                                              (loop (cdr ars) acc)
                                              (loop (cdr ars) (arity-at-least-value ar)))]
                                         [(number? ar)
                                          (loop (cdr ars) acc)]))]))])
         (and min-at-least
              (begin
                (let loop ([counts (sort (filter number? arity) >=)])
                  (unless (null? counts)
                    (let ([count (car counts)])
                      (cond
                        [(= (+ count 1) min-at-least)
                         (set! min-at-least count)
                         (loop (cdr counts))]
                        [(< count min-at-least)
                         (void)]
                        [else (loop (cdr counts))]))))
                (<= min-at-least dom-length))))])))

(define (check->* f arity-count)
  (unless (procedure? f)
    (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
  (unless (and (procedure-arity-includes? f arity-count)
               (no-mandatory-keywords? f))
    (error 'object-contract 
           "expected last argument of ->d* to be a procedure that accepts ~a arguments, got ~e"
           arity-count
           f)))

(define (get-mandatory-keywords f)
  (let-values ([(mandatory optional) (procedure-keywords f)])
    mandatory))

(define (no-mandatory-keywords? f)
  (let-values ([(mandatory optional) (procedure-keywords f)])
    (null? mandatory)))

(define (check->*/more f arity-count)
  (unless (procedure? f)
    (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
  (unless (procedure-accepts-and-more? f arity-count)
    (error 'object-contract 
           "expected last argument of ->d* to be a procedure that accepts ~a argument~a and arbitrarily many more, got ~e"
           arity-count
           (if (= 1 arity-count) "" "s")
           f)))


(define (check-pre-expr->pp/h val pre-expr blame)
  (unless pre-expr
    (raise-blame-error blame val "pre-condition expression failure")))

(define (check-post-expr->pp/h val post-expr blame)
  (unless post-expr
    (raise-blame-error blame val "post-condition expression failure")))

(define (check-procedure val dom-length optionals mandatory-kwds optional-keywords blame)
  (unless (and (procedure? val)
               (procedure-arity-includes?/optionals val dom-length optionals)
               (keywords-match mandatory-kwds optional-keywords val))
    (raise-blame-error
     blame
     val
     "expected a procedure that accepts ~a arguments~a, given: ~e"
     dom-length
     (keyword-error-text mandatory-kwds)
     val)))

(define (procedure-arity-includes?/optionals f base optionals)
  (cond
    [(zero? optionals) (procedure-arity-includes? f base)]
    [else (and (procedure-arity-includes? f (+ base optionals))
               (procedure-arity-includes?/optionals f base (- optionals 1)))]))

(define (keywords-match mandatory-kwds optional-kwds val)
  (let-values ([(proc-mandatory proc-all) (procedure-keywords val)])
    (and (equal? proc-mandatory mandatory-kwds)
         (andmap (λ (kwd) (and (member kwd proc-all)
                               (not (member kwd proc-mandatory))))
                 optional-kwds))))

(define (keyword-error-text mandatory-keywords)
  (cond
    [(null? mandatory-keywords) " without any keywords"]
    [(null? (cdr mandatory-keywords))
     (format " and the keyword ~a" (car mandatory-keywords))]
    [else
     (format
      " and the keywords ~a~a"
      (car mandatory-keywords)
      (apply string-append (map (λ (x) (format " ~a" x)) (cdr mandatory-keywords))))]))

(define ((check-procedure? arity) val)
  (and (procedure? val)
       (procedure-arity-includes? val arity)
       (no-mandatory-keywords? val)))

(define ((check-procedure/more? arity) val)
  (and (procedure? val)
       (procedure-accepts-and-more? val arity)))

(define (check-procedure/kind val arity kind-of-thing blame)
  (unless (procedure? val)
    (raise-blame-error blame val "expected a procedure, got ~e" val))
  (unless (procedure-arity-includes? val arity)
    (raise-blame-error blame
                       val
                       "expected a ~a of arity ~a (not arity ~a), got  ~e"
                       kind-of-thing
                       arity
                       (procedure-arity val)
                       val)))

(define (check-procedure/more/kind val arity kind-of-thing blame)
  (unless (procedure? val)
    (raise-blame-error blame val "expected a procedure, got ~e" val))
  (unless (procedure-accepts-and-more? val arity)
    (raise-blame-error blame
                       val
                       "expected a ~a that accepts ~a arguments and aribtrarily more (not arity ~a), got  ~e"
                       kind-of-thing
                       arity
                       (procedure-arity val)
                       val)))

(define (check-procedure/more val dom-length mandatory-kwds optional-kwds blame)
  (unless (and (procedure? val)
               (procedure-accepts-and-more? val dom-length)
               (keywords-match mandatory-kwds optional-kwds val))
    (raise-blame-error
     blame
     val
     "expected a procedure that accepts ~a arguments and arbitrarily more~a, given: ~e"
     dom-length
     (keyword-error-text mandatory-kwds)
     val)))


(define (check-rng-procedure who rng-x arity)
  (unless (and (procedure? rng-x)
               (procedure-arity-includes? rng-x arity))
    (error who "expected range position to be a procedure that accepts ~a arguments, given: ~e"
           arity
           rng-x)))

(define (check-rng-procedure/more rng-mk-x arity)
  (unless (and (procedure? rng-mk-x)
               (procedure-accepts-and-more? rng-mk-x arity))
    (error '->d* "expected range position to be a procedure that accepts ~a arguments and arbitrarily many more, given: ~e"
           arity 
           rng-mk-x)))

(define (check-rng-lengths results rng-contracts)
  (unless (= (length results) (length rng-contracts))
    (error '->d* 
           "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
           (length results) (length rng-contracts))))

#|

  test cases for procedure-accepts-and-more?

  (and (procedure-accepts-and-more? (lambda (x . y) 1) 3)
       (procedure-accepts-and-more? (lambda (x . y) 1) 2)
       (procedure-accepts-and-more? (lambda (x . y) 1) 1)
       (not (procedure-accepts-and-more? (lambda (x . y) 1) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 3)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 0)))
  
  |#
