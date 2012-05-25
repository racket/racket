
;;----------------------------------------------------------------------
;; record for static info produced by `define-struct'

(module struct-info '#%kernel
  (#%require "small-scheme.rkt")

  (#%provide make-struct-info
             struct-info?
             extract-struct-info
             struct:struct-info
             prop:struct-info

             prop:struct-auto-info
             struct-auto-info?
             struct-auto-info-lists)

  (define-values (prop:struct-info has-struct-info-prop? struct-info-prop-ref)
    (make-struct-type-property 'struct-info
                               (lambda (v type-info)
                                 (if (and (procedure? v)
                                          (procedure-arity-includes? v 1))
                                     v
                                     (raise-argument-error 'guard-for-prop:struct-info
                                                           "(any/c . -> . any/c)"
                                                           v)))))
  
  (define-values (struct:struct-info make-struct-info struct-info-rec?
                                     struct-info-ref struct-info-set!)
    (make-struct-type 'struct-info #f
                      1 0 #f
                      null (current-inspector)
                      (lambda (v stx)
                        (raise-syntax-error
                         #f
                         "identifier for static struct-type information cannot be used as an expression"
                         stx))
                      null
                      (lambda (proc info)
                        (if (and (procedure? proc)
                                 (procedure-arity-includes? proc 0))
                            proc
                            (raise-argument-error 'make-struct-info
                                                  "(procedure-arity-includes/c 0)"
                                                  proc)))))

  (define-values (extract-struct-info)
    (lambda (si)
      (cond
       [(struct-info-rec? si)
        (let ([p (struct-info-ref si 0)])
          (let ([v (p)])
            (if (struct-declaration-info? v)
                v
                (error 'extract-struct-info
                       "struct-info procedure result not properly formed: ~e"
                       v))))]
       [(has-struct-info-prop? si)
        (let ([v ((struct-info-prop-ref si) si)])
          (if (struct-declaration-info? v)
              v
              (error 'extract-struct-info
                     "prop:struct-info procedure result not properly formed: ~e"
                     v)))]
       [(set!-transformer? si)
        (extract-struct-info (set!-transformer-procedure si))]
       [(struct-declaration-info? si) si]
       [else (raise-argument-error 'extract-struct-info
                                   "struct-info?"
                                   si)])))

  (define-values (struct-info?)
    (lambda (si)
      (or (struct-info-rec? si)
          (and (has-struct-info-prop? si)
               (not (struct-type? si)))
          (struct-declaration-info? si)
          (and (set!-transformer? si)
               (struct-info-rec? (set!-transformer-procedure si))))))

  (define-values (struct-declaration-info?)
    (lambda (x)
      (letrec ([identifier?
                (lambda (x)
                  (and (syntax? x)
                       (symbol? (syntax-e x))))]
               [identifier/#f?
                (lambda (x)
                  (or (not x) 
                      (identifier? x)))]
               [id/#f-list?
                (lambda (id? x)
                  (or (null? x)
                      (and (pair? x)
                           (if (null? (cdr x))
                               (identifier/#f? (car x))
                               (and (id? (car x))
                                    (id/#f-list? id? (cdr x)))))))])
        (and (list? x)
             (= (length x) 6)
             (identifier/#f? (car x))
             (identifier/#f? (cadr x))
             (identifier/#f? (caddr x))
             (id/#f-list? identifier? (list-ref x 3))
             (id/#f-list? identifier/#f? (list-ref x 4))
             (= (length (list-ref x 3)) (length (list-ref x 4)))
             (or (eq? #t (list-ref x 5)) (identifier/#f? (list-ref x 5)))))))

  (define-values (prop:struct-auto-info
                  struct-auto-info?
                  struct-auto-info-ref)
    (make-struct-type-property 'struct-auto-info
                               (lambda (val info)
                                 (unless (and (procedure? val)
                                              (procedure-arity-includes? val 1))
                                   (raise-argument-error 'guard-for-prop:struct-auto-info "(procedure-arity-includes/c 1)" val))
                                 val)))

  (define-values (struct-auto-info-lists)
    (lambda (v)
      (unless (struct-auto-info? v)
        (raise-argument-error 'struct-auto-info-lists "struct-auto-info?" v))
      (let ([l ((struct-auto-info-ref v) v)]
            [identifier? (lambda (v) (and (syntax? v) (symbol? (syntax-e v))))])
        (unless (and (list? l)
                     (= 2 (length l))
                     (list? (car l))
                     (list? (cadr l))
                     (andmap identifier? (car l))
                     (andmap identifier? (cadr l)))
          (error 'struct-auto-info-lists
                 "struct-auto-info procedure result not properly formed: ~e"
                 l))
        l))))
