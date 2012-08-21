(module generic-interfaces "pre-base.rkt"

  ;; Defines (forgeries of) generic interfaces that correspond to struct
  ;; properties that come from racket/base.
  ;; Since racket/base can't depend on racket/generics, we can't use
  ;; `define-generics' to build these generic interfaces. Thus we must
  ;; forge them.

  (#%require (for-syntax '#%kernel))

  (#%provide gen:equal+hash gen:custom-write)

  (define-values (prop:gen:equal+hash equal+hash? gen:equal+hash-acc)
    (make-struct-type-property
     'prop:gen:equal+hash
     (lambda (v si)
       (if (and (vector? v)
                (= 3 (vector-length v))
                (procedure? (vector-ref v 0))
                (procedure-arity-includes? (vector-ref v 0) 3)
                (procedure? (vector-ref v 1))
                (procedure-arity-includes? (vector-ref v 1) 2)
                (procedure? (vector-ref v 2))
                (procedure-arity-includes? (vector-ref v 2) 2))
           v
           (raise-argument-error 'guard-for-prop:gen:equal+hash
                                 (string-append
                                  "(vector/c (procedure-arity-includes/c 3)\n"
                                  "          (procedure-arity-includes/c 2)\n"
                                  "          (procedure-arity-includes/c 2))")
                                 v)))
     (list (cons prop:equal+hash vector->list))))

  ;; forgeries of generic functions that don't exist
  (define (equal-proc a b e) (equal? a b))
  (define (hash-proc x h)  (equal-hash-code x))
  (define (hash2-proc x h) (equal-secondary-hash-code x))

  (define-syntax gen:equal+hash
    (list (quote-syntax prop:gen:equal+hash)
          (quote-syntax equal-proc)
          (quote-syntax hash-proc)
          (quote-syntax hash2-proc)))


  (define-values (prop:gen:custom-write gen:custom-write? gen:custom-write-acc)
    (make-struct-type-property
     'prop:gen:custom-write
     (lambda (v si)
       (if (and (vector? v)
                (= 1 (vector-length v))
                (procedure? (vector-ref v 0))
                (procedure-arity-includes? (vector-ref v 0) 3))
           v
           (raise-argument-error 'guard-for-prop:gen:custom-write
                                 "(vector/c (procedure-arity-includes/c 3))"
                                 v)))
     (list (cons prop:custom-write (lambda (v) (vector-ref v 0))))))

  ;; see above for equal+hash
  (define (write-proc v p w)
    (case w
      [(#t) (write v p)]
      [(#f) (display v p)]
      [(0 1) (print v p w)]
      [else (error 'write-proc "internal error; should not happen")]))

  (define-syntax gen:custom-write
    (list (quote-syntax prop:gen:custom-write)
          (quote-syntax write-proc)))

  )
