
(module old-procs '#%kernel
  (#%require "small-scheme.ss"
             "more-scheme.ss"
             "misc.ss"
             "stxmz-body.ss"
             "define.ss")

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
                  (raise-mismatch-error 'make-hash-table "bad argument: " a)))]
     [(a b) (if (or (and (eq? a 'equal)
                         (eq? b 'weak))
                    (and (eq? a 'weak)
                         (eq? b 'equal)))
                (make-weak-hash)
                (raise-mismatch-error 'make-hash-table "bad arguments: " (list a b)))]))

  (define make-immutable-hash-table
    (case-lambda
     [(l) (make-immutable-hasheq l)]
     [(l a) (if (eq? a 'equal)
                (make-immutable-hash l)
                (raise-mismatch-error 'make-immutable-hash-table "bad argument: " a))]))
  
  (define hash-table?
    (case-lambda
     [(v) (hash? v)]
     [(v a) (if (eq? a 'equal)
                (and (hash? v)
                     (not (hash-eq? v)))
                (if (eq? a 'weak)
                    (and (hash? v)
                         (hash-weak? v))
                    (raise-mismatch-error 'hash-table? "bad argument: " a)))]
     [(v a b) (if (or (and (eq? a 'equal)
                           (eq? b 'weak))
                      (and (eq? a 'weak)
                           (eq? b 'equal)))
                  (and (hash? v)
                       (not (hash-eq? v))
                       (hash-weak? v))
                  (raise-mismatch-error 'hash-table? "bad arguments: " (list a b)))])))
