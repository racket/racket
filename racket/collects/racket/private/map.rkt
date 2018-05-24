
;; #%kernel implements `map', `for-each', `andmap', and `ormap',
;;  but the JIT generates faster code, especially for the common cases.

(module map '#%kernel
  (#%require "small-scheme.rkt" "define.rkt"
             "performance-hint.rkt"
             "kw.rkt"
             '#%paramz
             (for-syntax '#%kernel))

  (#%provide (rename map2 map)
             (rename for-each2 for-each)
             (rename andmap2 andmap)
             (rename ormap2 ormap))

  (define-syntaxes (or-unsafe)
    (lambda (stx)
      (let-values ([(es) (cdr (syntax-e stx))])
        (let-values ([(e) (car (if (syntax? es)
                                   (syntax-e es)
                                   es))])
          (datum->syntax #f
                         (list (quote-syntax if)
                               (quote-syntax (variable-reference-from-unsafe? (#%variable-reference)))
                               (quote-syntax #t)
                               e))))))

  ;; -------------------------------------------------------------------------

  (begin-encourage-inline

   (define map2
      (let ([map
             (case-lambda
              [(f l)
               (if (or-unsafe (and (procedure? f)
                                   (procedure-arity-includes? f 1)
                                   (list? l)))
                   (let loop ([l l])
                     (cond
                      [(null? l) null]
                      [else
                       (let ([r (cdr l)]) ; so `l` is not necessarily retained during `f`
                         (cons (f (car l)) (loop r)))]))
                   (gen-map f (list l)))]
              [(f l1 l2)
               (if (or-unsafe
                    (and (procedure? f)
                         (procedure-arity-includes? f 2)
                         (list? l1)
                         (list? l2)
                         (= (length l1) (length l2))))
                   (let loop ([l1 l1] [l2 l2])
                     (cond
                      [(null? l1) null]
                      [else 
                       (let ([r1 (cdr l1)]
                             [r2 (cdr l2)])
                         (cons (f (car l1) (car l2)) 
                               (loop r1 r2)))]))
                   (gen-map f (list l1 l2)))]
              [(f l . args) (gen-map f (cons l args))])])
        map))
  
   (define for-each2
      (let ([for-each
             (case-lambda
              [(f l)
               (if (or-unsafe
                    (and (procedure? f)
                         (procedure-arity-includes? f 1)
                         (list? l)))
                   (let loop ([l l])
                     (cond
                      [(null? l) (void)]
                      [else
                       (let ([r (cdr l)])
                         (begin (f (car l)) (loop r)))]))
                   (gen-for-each f (list l)))]
              [(f l1 l2)
               (if (or-unsafe
                    (and (procedure? f)
                         (procedure-arity-includes? f 2)
                         (list? l1)
                         (list? l2)
                         (= (length l1) (length l2))))
                   (let loop ([l1 l1] [l2 l2])
                     (cond
                      [(null? l1) (void)]
                      [else
                       (let ([r1 (cdr l1)]
                             [r2 (cdr l2)])
                         (begin (f (car l1) (car l2)) 
                                (loop r1 r2)))]))
                   (gen-for-each f (list l1 l2)))]
              [(f l . args) (gen-for-each f (cons l args))])])
        for-each))

   (define andmap2
      (let ([andmap
             (case-lambda
              [(f l)
               (if (or-unsafe
                    (and (procedure? f)
                         (procedure-arity-includes? f 1)
                         (list? l)))
                   (if (null? l)
                       #t
                       (let loop ([l l])
                         (cond
                          [(null? (cdr l)) (f (car l))]
                          [else
                           (let ([r (cdr l)])
                             (and (f (car l))
                                  (loop r)))])))
                   (gen-andmap f (list l)))]
              [(f l1 l2)
               (if (or-unsafe
                    (and (procedure? f)
                         (procedure-arity-includes? f 2)
                         (list? l1)
                         (list? l2)
                         (= (length l1) (length l2))))
                   (if (null? l1)
                       #t
                       (let loop ([l1 l1] [l2 l2])
                         (cond
                          [(null? (cdr l1)) (f (car l1) (car l2))]
                          [else
                           (let ([r1 (cdr l1)]
                                 [r2 (cdr l2)])
                             (and (f (car l1) (car l2)) 
                                  (loop r1 r2)))])))
                   (gen-andmap f (list l1 l2)))]
              [(f l . args) (gen-andmap f (cons l args))])])
        andmap))

   (define ormap2
      (let ([ormap
             (case-lambda
              [(f l)
               (if (or-unsafe
                    (and (procedure? f)
                         (procedure-arity-includes? f 1)
                         (list? l)))
                   (if (null? l)
                       #f
                       (let loop ([l l])
                         (cond
                          [(null? (cdr l)) (f (car l))]
                          [else
                           (let ([r (cdr l)])
                             (or (f (car l)) (loop r)))])))
                   (gen-ormap f (list l)))]
              [(f l1 l2)
               (if (or-unsafe
                    (and (procedure? f)
                         (procedure-arity-includes? f 2)
                         (list? l1)
                         (list? l2)
                         (= (length l1) (length l2))))
                   (if (null? l1)
                       #f
                       (let loop ([l1 l1] [l2 l2])
                         (cond
                          [(null? (cdr l1)) (f (car l1) (car l2))]
                          [else 
                           (let ([r1 (cdr l1)]
                                 [r2 (cdr l2)])
                             (or (f (car l1) (car l2)) 
                                 (loop r1 r2)))])))
                   (gen-ormap f (list l1 l2)))]
              [(f l . args) (gen-ormap f (cons l args))])])
        ormap)))


  ;; -------------------------------------------------------------------------

  (define (check-args who f ls)
    (unless (procedure? f)
      (raise-argument-error who "procedure?" f))
    (let loop ([prev-len #f] [ls ls] [i 1])
      (unless (null? ls)
        (let ([l (car ls)])
          (unless (list? l)
            (raise-argument-error who "list?" l))
          (let ([len (length l)])
            (when (and prev-len
                       (not (= len prev-len)))
              (raise-arguments-error who "all lists must have same size"
                                     "first list length" prev-len
                                     "other list length" len
                                     "procedure" f))
            (loop len (cdr ls) (add1 i))))))
    (unless (procedure-arity-includes? f (length ls))
      (define-values (required-keywords optional-keywords) (procedure-keywords f))
      (apply raise-arguments-error who
             (if (pair? required-keywords)
                 (string-append "argument mismatch;\n"
                                " the given procedure expects keyword arguments")
                 (string-append "argument mismatch;\n"
                                " the given procedure's expected number of arguments does not match"
                                " the given number of lists"))
             "given procedure" (unquoted-printing-string
                                (or (let ([n (object-name f)])
                                      (and (symbol? n)
                                           (symbol->string n)))
                                    "#<procedure>"))
             (append
              (let ([a (procedure-arity f)])
                (cond
                  [(pair? required-keywords)
                   null]
                  [(integer? a)
                   (list "expected" a)]
                  [(arity-at-least? a)
                   (list "expected" (unquoted-printing-string
                                     (string-append "at least " (number->string (arity-at-least-value a)))))]
                  [else
                   null]))
              (cond
                [(pair? required-keywords)
                 null]
                [else
                 (list "given" (length ls))])
              (cond
                [(pair? required-keywords)
                 (list "required keywords"
                       (unquoted-printing-string
                        (apply string-append
                               (cdr
                                (let loop ([kws required-keywords])
                                  (cond
                                    [(null? kws) null]
                                    [else (list* " "
                                                 (string-append "#:"
                                                                (keyword->string (car kws)))
                                                 (loop (cdr kws)))]))))))]
                [else
                 null])
              (let ([w (quotient (error-print-width) (length ls))])
                (if (w . > . 10)
                    (list "argument lists..."
                          (unquoted-printing-string
                           (apply string-append
                                  (let loop ([ls ls])
                                    (cond
                                      [(null? ls) null]
                                      [else (cons (string-append "\n   "
                                                                 ((error-value->string-handler)
                                                                  (car ls)
                                                                  w))
                                                  (loop (cdr ls)))])))))
                    null))))))
  
  (define (gen-map f ls)
    (or-unsafe (check-args 'map f ls))
    (let loop ([ls ls])
      (cond
        [(null? (car ls)) null]
        [else
         (let ([next-ls (map2 cdr ls)])
           (cons (apply f (map2 car ls))
                 (loop next-ls)))])))

  (define (gen-for-each f ls)
    (or-unsafe (check-args 'for-each f ls))
    (let loop ([ls ls])
      (unless (null? (car ls))
        (let ([next-ls (map2 cdr ls)])
          (apply f (map2 car ls))
          (loop next-ls)))))

  (define (gen-andmap f ls)
    (or-unsafe (check-args 'andmap f ls))
    (let loop ([ls ls])
      (cond
        [(null? (car ls)) #t]
        [(null? (cdar ls)) (apply f (map2 car ls))]
        [else (let ([next-ls (map2 cdr ls)])
                (and (apply f (map2 car ls))
                     (loop next-ls)))])))

  (define (gen-ormap f ls)
    (or-unsafe (check-args 'ormap f ls))
    (let loop ([ls ls])
      (cond
        [(null? (car ls)) #f]
        [(null? (cdar ls)) (apply f (map2 car ls))]
        [else (let ([next-ls (map2 cdr ls)])
                (or (apply f (map2 car ls))
                    (loop next-ls)))])))

  (void))
