(module for-base '#%kernel

  (#%require "more-scheme.rkt"
             "misc.rkt"
             "define.rkt"
             "letstx-scheme.rkt"
             '#%unsafe
             (for-syntax '#%kernel
                         "stx.rkt"
                         "qqstx.rkt"
                         "define.rkt"
                         "small-scheme.rkt"
                         "stxcase-scheme.rkt"))

  (#%provide struct:do-sequence
             make-do-sequence
             do-sequence?
             do-sequence-ref
             do-sequence-set!

             :do-in
             
             prop:sequence

             define-sequence-syntax

             sequence?
             :sequence?
             :sequence-ref

             (for-syntax struct:sequence-transformer
                         make-sequence-transformer
                         sequence-transformer?
                         sequence-transformer-ref
                         sequence-transformer-set!
                         
                         create-sequence-transformer))
  
  (begin-for-syntax
    (define-values (struct:sequence-transformer
                    make-sequence-transformer
                    sequence-transformer?
                    sequence-transformer-ref
                    sequence-transformer-set!)
      (make-struct-type 'sequence-transformer #f
                        3 0 #f
                        null (current-inspector)
                        0))

    (define (create-sequence-transformer proc1 proc2 cert)
      (unless (and (procedure? proc1)
                   (or (procedure-arity-includes? proc1 1)
                       (procedure-arity-includes? proc1 0)))
        (raise-type-error 'define-sequence-syntax
                          "procedure (arity 0 or 1)"
                          0
                          proc1 proc2))
      (unless (and (procedure? proc2)
                   (procedure-arity-includes? proc2 1))
        (raise-type-error 'define-sequence-syntax
                          "procedure (arity 1)"
                          1
                          proc1 proc2))
      (make-sequence-transformer
       (if (procedure-arity-includes? proc1 0)
         (lambda (stx)
           (if (identifier? stx)
             (proc1)
             (datum->syntax stx
                            #`(#,(proc1) . #,(cdr (syntax-e stx)))
                            stx
                            stx)))
         proc1)
       proc2
       cert))
    )

  (define-syntax (:do-in stx)
    (raise-syntax-error #f
      "illegal outside of a loop or comprehension binding" stx))
  
  (define-values (struct:do-sequence
                  make-do-sequence
                  do-sequence?
                  do-sequence-ref
                  do-sequence-set!)
    (make-struct-type 'sequence #f 1 0 #f))

  (define-values (prop:sequence :sequence? :sequence-ref)
    (make-struct-type-property
     'sequence
     (lambda (v sinfo)
       (unless (and (procedure? v) (procedure-arity-includes? v 1))
         (raise-type-error 'sequence-property-guard "procedure (arity 1)" v))
       (lambda (self)
         (let ([s (v self)])
           (unless (sequence? s)
             (raise-mismatch-error
              'sequence-generate
              "procedure (value of prop:sequence) produced a non-sequence: "
              s))
           s)))))

  (define-syntax define-sequence-syntax
    (syntax-rules ()
      [(_ id expr-transformer-expr clause-transformer-expr)
       (define-syntax id
         (create-sequence-transformer expr-transformer-expr
                                      clause-transformer-expr
                                      (syntax-local-certifier #f)))]))

  (define (sequence? v)
    (or (do-sequence? v)
        (list? v)
        (mpair? v)
        (vector? v)
        (string? v)
        (bytes? v)
        (input-port? v)
        (hash? v)
        (and (:sequence? v) (not (struct-type? v)))))
  
  )

  