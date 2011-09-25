
(module old-procs '#%kernel
  (#%require "small-scheme.rkt"
             "more-scheme.rkt"
             "define.rkt")

  (#%provide make-namespace
             free-identifier=?*
             namespace-transformer-require
             transcript-on
             transcript-off
             make-hash-table
             make-immutable-hash-table
             hash-table?)

  (define reflect-var #f)
  
  (define make-namespace
    (case-lambda
     [() (make-namespace 'initial)]
     [(flag)
      (unless (memq flag '(initial empty))
        (raise-syntax-error 'make-namespace
                            "'initial or 'empty"
                            flag))
      (let* ([old (variable-reference->empty-namespace (#%variable-reference reflect-var))]
             [new (parameterize ([current-namespace old])
                    (make-empty-namespace))])
        (namespace-attach-module old 'mzscheme new)
        (unless (eq? flag 'empty)
          (parameterize ([current-namespace new])
            (namespace-require/copy 'mzscheme)))
        new)]))

  (define (free-identifier=?* a b)
    (and (eq? (syntax-e a)
              (syntax-e b))
         (free-identifier=? a b)))

  (define (namespace-transformer-require qrs)
    (namespace-require `(for-syntax ,qrs)))

  (define (transcript-on filename)
    (error 'transcript-on "unsupported"))
  (define (transcript-off)
    (error 'transcript-off "unsupported"))

  (define make-hash-table
    (case-lambda
     [() (make-hasheq)]
     [(a) (if (eq? a 'equal)
              (make-hash)
              (if (eq? a 'weak)
                  (make-weak-hasheq)
                  (if (eq? a 'eqv)
                      (make-hasheqv)
                      (raise-mismatch-error 'make-hash-table "bad argument: " a))))]
     [(a b) (if (or (and (or (eq? a 'equal)
                             (eq? a 'eqv))
                         (eq? b 'weak))
                    (and (eq? a 'weak)
                         (or (eq? b 'equal)
                             (eq? b 'eqv))))
                (if (or (eq? a 'eqv) (eq? b 'eqv))
                    (make-weak-hasheqv)
                    (make-weak-hash))
                (raise-mismatch-error 'make-hash-table "bad arguments: " (list a b)))]))

  (define make-immutable-hash-table
    (case-lambda
     [(l) (make-immutable-hasheq l)]
     [(l a) (if (eq? a 'equal)
                (make-immutable-hash l)
                (if (eq? a 'eqv)
                    (make-immutable-hasheqv l)
                    (raise-mismatch-error 'make-immutable-hash-table "bad argument: " a)))]))
  
  (define hash-table?
    (case-lambda
     [(v) (hash? v)]
     [(v a) (if (eq? a 'equal)
                (and (hash? v)
                     (not (hash-eq? v))
                     (not (hash-eqv? v)))
                (if (eq? a 'weak)
                    (and (hash? v)
                         (hash-weak? v))
                    (if (eq? a 'eqv)
                        (hash-eqv? v)
                        (raise-mismatch-error 'hash-table? "bad argument: " a))))]
     [(v a b) (if (or (and (or (eq? a 'equal) (eq? a 'eqv))
                           (eq? b 'weak))
                      (and (eq? a 'weak)
                           (or (eq? b 'equal) (eq? b 'eqv))))
                  (and (hash? v)
                       (if (or (eq? a 'eqv) (eq? b 'eqv))
                           (hash-eqv? v)
                           (not (or (hash-eq? v) (hash-eqv? v))))
                       (hash-weak? v))
                  (raise-mismatch-error 'hash-table? "bad arguments: " (list a b)))])))
