
;; #%kernel implements `map', `for-each', `andmap', and `ormap',
;;  but the JIT generates faster code, especially for the common cases.

(module map '#%kernel
  (#%require '#%utils ; built into mzscheme
             "small-scheme.rkt" "define.rkt"
             "performance-hint.rkt")

  (#%provide (rename map2 map)
             (rename for-each2 for-each)
             (rename andmap2 andmap)
             (rename ormap2 ormap))
  
  ;; -------------------------------------------------------------------------

  (begin-encourage-inline

   (define map2
      (let ([map
             (case-lambda
              [(f l)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 1)
                        (list? l))
                   (let loop ([l l])
                     (cond
                      [(null? l) null]
                      [else (cons (f (car l)) (loop (cdr l)))]))
                   (map f l))]
              [(f l1 l2)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 2)
                        (list? l1)
                        (list? l2)
                        (= (length l1) (length l2)))
                   (let loop ([l1 l1][l2 l2])
                     (cond
                      [(null? l1) null]
                      [else (cons (f (car l1) (car l2)) 
                                  (loop (cdr l1) (cdr l2)))]))
                   (map f l1 l2))]
              [(f l . args) (apply map f l args)])])
        map))
  
   (define for-each2
      (let ([for-each
             (case-lambda
              [(f l)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 1)
                        (list? l))
                   (let loop ([l l])
                     (cond
                      [(null? l) (void)]
                      [else (begin (f (car l)) (loop (cdr l)))]))
                   (for-each f l))]
              [(f l1 l2)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 2)
                        (list? l1)
                        (list? l2)
                        (= (length l1) (length l2)))
                   (let loop ([l1 l1][l2 l2])
                     (cond
                      [(null? l1) (void)]
                      [else (begin (f (car l1) (car l2)) 
                                   (loop (cdr l1) (cdr l2)))]))
                   (for-each f l1 l2))]
              [(f l . args) (apply for-each f l args)])])
        for-each))

   (define andmap2
      (let ([andmap
             (case-lambda
              [(f l)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 1)
                        (list? l))
                   (if (null? l)
                       #t
                       (let loop ([l l])
                         (cond
                          [(null? (cdr l)) (f (car l))]
                          [else (and (f (car l)) (loop (cdr l)))])))
                   (andmap f l))]
              [(f l1 l2)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 2)
                        (list? l1)
                        (list? l2)
                        (= (length l1) (length l2)))
                   (if (null? l1)
                       #t
                       (let loop ([l1 l1][l2 l2])
                         (cond
                          [(null? (cdr l1)) (f (car l1) (car l2))]
                          [else (and (f (car l1) (car l2)) 
                                     (loop (cdr l1) (cdr l2)))])))
                   (andmap f l1 l2))]
              [(f l . args) (apply andmap f l args)])])
        andmap))

   (define ormap2
      (let ([ormap
             (case-lambda
              [(f l)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 1)
                        (list? l))
                   (if (null? l)
                       #f
                       (let loop ([l l])
                         (cond
                          [(null? (cdr l)) (f (car l))]
                          [else (or (f (car l)) (loop (cdr l)))])))
                   (ormap f l))]
              [(f l1 l2)
               (if (and (procedure? f)
                        (procedure-arity-includes? f 2)
                        (list? l1)
                        (list? l2)
                        (= (length l1) (length l2)))
                   (if (null? l1)
                       #f
                       (let loop ([l1 l1][l2 l2])
                         (cond
                          [(null? (cdr l1)) (f (car l1) (car l2))]
                          [else (or (f (car l1) (car l2)) 
                                    (loop (cdr l1) (cdr l2)))])))
                   (ormap f l1 l2))]
              [(f l . args) (apply ormap f l args)])])
        ormap))))
